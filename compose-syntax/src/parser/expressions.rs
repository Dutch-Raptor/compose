use crate::ast::{AssignOp, BinOp};
use crate::kind::SyntaxKind;
use crate::parser::control_flow::{conditional, for_loop, while_loop};
use crate::parser::statements::statement;
use crate::parser::{funcs, statements, ExprContext};
use crate::parser::{Marker, Parser};
use crate::precedence::{Precedence, PrecedenceTrait};
use crate::set::{syntax_set, SyntaxSet, UNARY_OP};
use crate::{ast, Label};
use compose_error_codes::{E0001_UNCLOSED_DELIMITER, E0002_INVALID_ASSIGNMENT};
use compose_utils::{trace_fn, trace_log};
use ecow::eco_format;

pub fn code(p: &mut Parser, end_set: SyntaxSet) {
    let mut pos = p.current_end();
    while !p.end() && !p.at_set(end_set) {
        trace_fn!("code", "loop pos= {}", pos);
        statement(p);

        p.skip_if(SyntaxKind::Semicolon);

        // If the parser is not progressing, then we have an error
        if pos == p.current_end() && !p.end() {
            // Eat the token and continue trying to parse
            p.unexpected("Expected a statement", None);
        }
        pos = p.current_end();
    }
}

pub fn code_expression(p: &mut Parser) {
    code_expr_prec(p, ExprContext::Expr, Precedence::Lowest);
}

pub fn code_expr_prec(p: &mut Parser, ctx: ExprContext, min_prec: Precedence) {
    trace_fn!("parse_code_expr_prec", "{ctx:?} {}", p.current_end());

    // handle infinite loops
    if p.current_end() == p.last_pos {
        p.eat();
        return;
    }
    p.last_pos = p.current_end();

    let m = p.marker();
    if !ctx.is_atomic() && p.at_set(UNARY_OP) {
        let op = ast::UnOp::from_kind(p.current()).expect("Was checked to be a unary op");
        p.eat();
        code_expr_prec(p, ExprContext::AtomicExpr, op.precedence());
        p.wrap(m, SyntaxKind::Unary)
    } else {
        primary_expr(p, ctx.to_expr());
    }

    loop {
        trace_fn!("parse_code_expr_prec loop", "{:?}", p.current());
        if p.at(SyntaxKind::LeftParen) {
            funcs::args(p);
            p.wrap(m, SyntaxKind::FuncCall);
            continue;
        }

        let at_field_or_index = p.at(SyntaxKind::Dot) || p.at(SyntaxKind::LeftBracket);

        if ctx.is_atomic() && !at_field_or_index {
            break;
        }

        // handle path access `a::b::c`
        if p.eat_if(SyntaxKind::ColonColon) {
            p.expect(SyntaxKind::Ident);
            p.wrap(m, SyntaxKind::PathAccess);
            continue;
        }

        // Handle field access `a.b.c`
        if p.eat_if(SyntaxKind::Dot) {
            p.expect(SyntaxKind::Ident);
            p.wrap(m, SyntaxKind::FieldAccess);
            continue;
        }

        // Handle index access `a[2]`
        if p.eat_if(SyntaxKind::LeftBracket) {
            code_expression(p);
            p.expect(SyntaxKind::RightBracket);
            p.wrap(m, SyntaxKind::IndexAccess)
        }

        let bin_op = BinOp::from_kind(p.current());

        if let Some(op) = bin_op {
            let prec = op.precedence();
            if prec < min_prec {
                break;
            }

            p.eat();
            code_expr_prec(p, ExprContext::Expr, prec);
            p.wrap(m, SyntaxKind::Binary);
            continue;
        }

        if ctx.is_expr() && AssignOp::from_kind(p.current()).is_some() {
            err_assign_in_expr_context(p);
        }

        break;
    }
}

fn err_assign_in_expr_context(p: &mut Parser) {
    let lhs_text = p
        .last_text()
        .map(ToOwned::to_owned)
        .unwrap_or_else(|| "a".into());

    let error_marker = p.marker();

    p.insert_error_here("assignments are not allowed in expression contexts")
        .with_code(&E0002_INVALID_ASSIGNMENT)
        .with_label_message("assignment is not allowed here");
    p.eat();
    code_expression(p);

    let rhs_text = p
        .last_text()
        .map(ToOwned::to_owned)
        .unwrap_or_else(|| "b".into());

    let err = p.err_at(error_marker).expect("An error was added");
    err.with_note(eco_format!(
        "assignments like `{lhs_text} = ...` are only valid as standalone statements"
    ));
    err.with_hint(eco_format!(
        "if you meant to compare `{lhs_text}` and `{rhs_text}`, use `==` instead of `=`"
    ));
    err.with_hint(eco_format!("if you meant to assign to `{lhs_text}`, consider using `let` for new bindings or wrapping the assignment in a block: `{{ {lhs_text} = ... }}`"));
}

/// Parse a primary expression.
///
/// A primary expressions are the building blocks in composable expressions.
fn primary_expr(p: &mut Parser, ctx: ExprContext) {
    trace_fn!("parse_primary_expr");
    let m = p.marker();
    match p.current() {
        SyntaxKind::Ident => {
            trace_fn!("parse_primary_expr: ident");
            p.eat();
            // Parse a closure like `a => a + 1`
            if !ctx.is_atomic() && p.at(SyntaxKind::Arrow) {
                p.wrap(m, SyntaxKind::Params);
                p.assert(SyntaxKind::Arrow);
                code_expression(p);
                p.wrap(m, SyntaxKind::Closure)
            }
        }
        SyntaxKind::Underscore if !ctx.is_atomic() => {
            trace_fn!("parse_primary_expr: underscore");
            // Parse closure like `_ => 1` or destructure like `_ = foo()`
            if p.at(SyntaxKind::Arrow) {
                p.wrap(m, SyntaxKind::Params);
                p.assert(SyntaxKind::Arrow);
                code_expression(p);
                p.wrap(m, SyntaxKind::Closure)
            } else if p.eat_if(SyntaxKind::Eq) {
                code_expression(p);
                p.wrap(m, SyntaxKind::DestructureAssignment);
            } else {
                err_expected_expression(p);
            }
        }
        SyntaxKind::LeftBrace => block(p),
        SyntaxKind::LeftParen if at_unit_literal(p) => {
            trace_fn!("parse_primary_expr: unit literal");
            p.assert(SyntaxKind::LeftParen);
            p.assert(SyntaxKind::RightParen);
            p.wrap(m, SyntaxKind::Unit)
        }
        SyntaxKind::If => conditional(p),
        SyntaxKind::While => while_loop(p),
        SyntaxKind::For => for_loop(p),
        SyntaxKind::LeftParen => expr_with_parens(p, ctx),
        SyntaxKind::Let => statements::let_binding(p),
        // Already fully handled in the lexer
        SyntaxKind::Int | SyntaxKind::Float | SyntaxKind::Bool | SyntaxKind::Str => p.eat(),
        _ => err_expected_expression(p),
    }
}

fn block(p: &mut Parser) {
    trace_fn!("parse_block");
    let m = p.marker();
    p.assert(SyntaxKind::LeftBrace);

    code(p, syntax_set!(End, RightBrace, RightParen, RightBracket));

    p.expect_closing_delimiter(m, SyntaxKind::RightBrace);
    p.wrap(m, SyntaxKind::CodeBlock)
}

fn err_expected_expression(p: &mut Parser) {
    p.unexpected(
        "expected the start of an expression",
        Some(syntax_set!(NewLine, Semicolon, RightBrace)),
    )
    .with_label_message("expected an expression here")
    .with_hint("expressions can be values, operations, function calls, blocks, etc.");
}

fn expr_with_parens(p: &mut Parser, ctx: ExprContext) {
    trace_fn!("parse_expr_with_parens");
    if ctx.is_atomic() {
        // If atomic, we don't parse any of the more complicated expressions
        parenthesized(p);
        return;
    }

    // Since expressions with parens all look alike at the start of the expression, we will have to
    // guess and backtrack if we guess incorrectly.
    let Some((memo_key, checkpoint)) = p.restore_memo_or_checkpoint() else {
        trace_log!("parse_expr_with_parens: restored from memo");
        return;
    };

    let prev_len = checkpoint.nodes_len;

    // The most common and simple expr with parens is the parenthesized expression, so we try that first.
    parenthesized(p);

    if p.at(SyntaxKind::Arrow) {
        trace_fn!("parse_expr_with_parens: found closure");
        // It looks like it was a closure instead! Let's rewind and parse as a closure instead
        p.restore(checkpoint);
        funcs::closure(p)
    }

    p.memoize_parsed_nodes(memo_key, prev_len)
}

fn at_unit_literal(p: &Parser) -> bool {
    let mut peeker = p.peeker();
    let cur = p.current();
    let next = peeker.next().unwrap_or(SyntaxKind::End);
    let next_next = peeker.next().unwrap_or(SyntaxKind::End);

    matches!(cur, SyntaxKind::LeftParen)
        && matches!(next, SyntaxKind::RightParen)
        && !matches!(next_next, SyntaxKind::Arrow)
}

fn parenthesized(p: &mut Parser) {
    trace_fn!("parse_parenthesized");
    let m = p.marker();
    p.assert(SyntaxKind::LeftParen);
    code_expr_prec(p, ExprContext::Expr, Precedence::Lowest);
    if !p.expect_closing_delimiter(m, SyntaxKind::RightParen) {
        // If the closing parenthesis is missing or incorrect, try to recover
        p.recover_until(syntax_set!(RightParen, NewLine, Semicolon));
        p.eat_if(SyntaxKind::RightParen);
    }
    p.wrap(m, SyntaxKind::Parenthesized)
}

pub(in crate::parser) fn err_unclosed_delim(
    p: &mut Parser,
    open_marker: Marker,
    expected_closing: SyntaxKind,
) {
    let closing_delim_label;
    if p.current().is_closing_delimiter() {
        closing_delim_label = eco_format!(
            "unexpected closing delimiter `{}`",
            p.current().descriptive_name()
        );
    } else if p.end() {
        closing_delim_label = eco_format!(
            "expected a closing `{}` but reached the end of the file instead",
            expected_closing.descriptive_name()
        );
    } else {
        closing_delim_label = eco_format!(
            "expected closing `{}`, but found `{}` instead",
            expected_closing.descriptive_name(),
            p.current_text()
        );
    }
    let closing_span = p.current_span();
    p[open_marker]
        .convert_to_error("unclosed delimiter")
        .with_code(&E0001_UNCLOSED_DELIMITER)
        // label on the opening delimiter
        .with_label_message(eco_format!(
            "unclosed `{}` starts here",
            expected_closing.matching_delimiter().unwrap().descriptive_name()
        ))
        // label on (or near) the closing delimiter / EOF
        .with_label(Label::primary(closing_span, closing_delim_label))
        // trailing note
        .with_note(eco_format!(
            "expected `{}` to match the opening delimiter",
            expected_closing.descriptive_name()
        ));
}