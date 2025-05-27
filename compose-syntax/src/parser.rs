use crate::ast::BinOp;
use crate::file::FileId;
use crate::kind::SyntaxKind;
use crate::node::SyntaxNode;
use crate::parser_impl::{Marker, Parser};
use crate::precedence::{Precedence, PrecedenceTrait};
use crate::set::{ARG_RECOVER, UNARY_OP, syntax_set};
use crate::{ast, set};
use compose_utils::{trace_fn, trace_log};
use ecow::eco_format;
use std::collections::HashSet;

pub fn parse(text: &str, file_id: FileId) -> Vec<SyntaxNode> {
    parse_with_offset(text, file_id, 0)
}

/// Assumes that the text at offset begins with a valid expression (or whitespace).
pub fn parse_with_offset(text: &str, file_id: FileId, offset: usize) -> Vec<SyntaxNode> {
    let mut p = Parser::new(text, offset, file_id);

    let mut pos = p.current_end();
    while !p.end() {
        code_expression(&mut p);

        p.skip_if(SyntaxKind::Semicolon);

        // If the parser is not progressing, then we have an error
        if pos == p.current_end() && !p.end() {
            // Eat the token and continue trying to parse
            p.unexpected("", None);
        }
        pos = p.current_end();
    }

    p.finish()
}

fn code_expression(p: &mut Parser) {
    code_expr_prec(p, false, Precedence::Lowest);
}

fn code_expr_prec(p: &mut Parser, atomic: bool, min_prec: Precedence) {
    trace_fn!(
        "parse_code_expr_prec",
        "atomic: {atomic} {}",
        p.current_end()
    );

    // handle infinite loops
    if p.current_end() == p.last_pos {
        p.eat();
        return;
    }
    p.last_pos = p.current_end();

    let m = p.marker();
    if !atomic && p.at_set(UNARY_OP) {
        let op = ast::UnOp::from_kind(p.current()).expect("Was checked to be a unary op");
        p.eat();
        code_expr_prec(p, true, op.precedence());
        p.wrap(m, SyntaxKind::Unary)
    } else {
        primary_expr(p, atomic);
    }

    loop {
        trace_fn!("parse_code_expr_prec loop", "{:?}", p.current());
        if p.at(SyntaxKind::LeftParen) {
            args(p);
            p.wrap(m, SyntaxKind::FuncCall);
            continue;
        }

        let at_field_or_index = p.at(SyntaxKind::Dot) || p.at(SyntaxKind::LeftBracket);

        if atomic && !at_field_or_index {
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
            code_expr_prec(p, false, prec);
            p.wrap(m, SyntaxKind::Binary);
            continue;
        }

        break;
    }
}

fn args(p: &mut Parser) {
    trace_fn!("parse_args");
    let m = p.marker();
    p.expect(SyntaxKind::LeftParen);

    while !p.current().is_terminator() {
        arg(p);

        if !p.current().is_terminator() {
            p.expect_or_recover(SyntaxKind::Comma, ARG_RECOVER);
        }
    }

    p.expect_closing_delimiter(m, SyntaxKind::RightParen);

    p.wrap(m, SyntaxKind::Args);
}

fn arg(p: &mut Parser) {
    let m = p.marker();
    code_expression(p);

    if p.eat_if(SyntaxKind::Colon) {
        if p[m].kind() != SyntaxKind::Ident {
            p[m].expected("identifier");
        }
        code_expression(p);
        p.wrap(m, SyntaxKind::Named)
    }
}

/// Parse a primary expression.
///
/// A primary expressions are the building blocks in composable expressions.
fn primary_expr(p: &mut Parser, atomic: bool) {
    trace_fn!("parse_primary_expr");
    let m = p.marker();
    match p.current() {
        SyntaxKind::Ident => {
            trace_fn!("parse_primary_expr: ident");
            p.eat();
            // Parse a closure like `a => a + 1`
            if !atomic && p.at(SyntaxKind::Arrow) {
                p.wrap(m, SyntaxKind::Params);
                p.assert(SyntaxKind::Arrow);
                code_expression(p);
                p.wrap(m, SyntaxKind::Closure)
            }
        }
        SyntaxKind::Underscore if !atomic => {
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
                p[m].expected("expression")
            }
        }
        SyntaxKind::LeftBrace => block(p),
        SyntaxKind::LeftParen if at_unit_literal(p) => {
            trace_fn!("parse_primary_expr: unit literal");
            p.assert(SyntaxKind::LeftParen);
            p.assert(SyntaxKind::RightParen);
            p.wrap(m, SyntaxKind::Unit)
        }
        SyntaxKind::LeftParen => expr_with_parens(p, atomic),
        SyntaxKind::Let => let_binding(p),
        // Already fully handled in the lexer
        SyntaxKind::Int | SyntaxKind::Float | SyntaxKind::Bool | SyntaxKind::Str => p.eat(),
        _ => p.expected("expression"),
    }
}

fn expr_with_parens(p: &mut Parser, atomic: bool) {
    trace_fn!("parse_expr_with_parens");
    if atomic {
        // If atomic, we don't parse any of the more complicated expressions
        parenthesized(p);
        return;
    }

    // Since expressions with params all look alike at the start of the expression, we will have to
    // guess and backtrack if we guess incorrectly.
    let Some((memo_key, checkpoint)) = p.restore_memo_or_checkpoint() else {
        trace_log!("parse_expr_with_parens: restored from memo");
        return;
    };
    
    let prev_len = checkpoint.node_len;

    // The most common and simple expr with params is the parenthesized expression, so we try that first.
    parenthesized(p);

    if p.at(SyntaxKind::Arrow) {
        trace_fn!("parse_expr_with_parens: found closure");
        // It looks like it was a closure instead! Let's rewind and parse as a closure instead
        p.restore(checkpoint);
        closure(p)
    }

    p.memoize_parsed_nodes(memo_key, prev_len)
}

fn closure(p: &mut Parser) {
    trace_fn!("parse_closure");
    debug_assert_eq!(p.current(), SyntaxKind::LeftParen);
    let m = p.marker();

    params(p);

    p.expect_or_recover_until(
        SyntaxKind::Arrow,
        "expected `=>` after closure parameters",
        syntax_set!(Arrow, LeftBrace, NewLine),
    )
    .map(|e| {
        e.with_label_message("help: you probably meant to write `=>` here");
    });

    code_expression(p);

    p.wrap(m, SyntaxKind::Closure)
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

/// Parse closure parameters.
fn params(p: &mut Parser) {
    let m = p.marker();
    p.assert(SyntaxKind::LeftParen);

    let mut seen = HashSet::new();

    while !p.current().is_terminator() {
        if !p.at_set(set::PARAM) {
            p.unexpected("expected a param", None);
            continue;
        }

        param(p, &mut seen);

        if !p.current().is_terminator() {
            p.expect_or_recover(SyntaxKind::Comma, ARG_RECOVER);
        }
    }

    p.expect_closing_delimiter(m, SyntaxKind::RightParen);
    p.wrap(m, SyntaxKind::Params)
}

fn param<'s>(p: &mut Parser<'s>, seen: &mut HashSet<&'s str>) {
    trace_fn!("parse_param");
    let m = p.marker();

    let was_at_pat = p.at_set(set::PATTERN);
    pattern(p, false, seen, Some("parameter"));

    // Parse named params like `a = 1`
    if p.eat_if(SyntaxKind::Eq) {
        if was_at_pat && p[m].kind() != SyntaxKind::Ident {
            p[m].expected("identifier");
        }

        code_expression(p);
        p.wrap(m, SyntaxKind::Named)
    }
}

/// Parses a binding or reassignment pattern.
fn pattern<'s>(
    p: &mut Parser<'s>,
    reassignment: bool,
    seen: &mut HashSet<&'s str>,
    dupe: Option<&'s str>,
) {
    trace_fn!("parse_pattern");
    match p.current() {
        SyntaxKind::Underscore => p.eat(),
        SyntaxKind::At => destructuring(p, reassignment, seen, dupe),
        _ => pattern_leaf(p, reassignment, seen, dupe),
    }
}

fn pattern_leaf<'s>(
    p: &mut Parser<'s>,
    reassignment: bool,
    seen: &mut HashSet<&'s str>,
    dupe: Option<&'s str>,
) {
    trace_fn!("parse_pattern_leaf");
    if p.current().is_keyword() {
        p.token.node.expected("pattern");
        p.eat();
        return;
    } else if !p.at_set(set::PATTERN_LEAF) {
        p.expected("pattern");
        return;
    }

    let m = p.marker();
    let text = p.current_text();

    // Parse a full expression, even though we only care about an identifier.
    // This way the entire expression can be marked as an error if it is not.
    code_expr_prec(p, true, Precedence::Lowest);

    // If the pattern is not a reassignment, it can only be an identifier
    if !reassignment {
        let node = &mut p[m];
        if node.kind() != SyntaxKind::Ident {
            node.expected("pattern");
            return;
        }
        if !seen.insert(text) {
            node.convert_to_error(eco_format!(
                "duplicate {binding}: {text}",
                binding = dupe.unwrap_or("binding")
            ))
        }
    }
}

fn destructuring(p: &mut Parser, reassignment: bool, seen: &mut HashSet<&str>, dupe: Option<&str>) {
    trace_fn!("parse_destructuring");
    p.assert(SyntaxKind::At);

    unimplemented!("destructuring")
}

fn let_binding(p: &mut Parser) {
    trace_fn!("parse_let_binding");
    let m = p.marker();
    p.assert(SyntaxKind::Let);

    p.eat_if(SyntaxKind::Mut);

    pattern(p, false, &mut HashSet::new(), None);

    if p.eat_if(SyntaxKind::Eq) {
        code_expression(p);
    }

    p.wrap(m, SyntaxKind::LetBinding)
}

fn parenthesized(p: &mut Parser) {
    trace_fn!("parse_parenthesized");
    let m = p.marker();
    p.assert(SyntaxKind::LeftParen);
    code_expr_prec(p, false, Precedence::Lowest);
    p.assert(SyntaxKind::RightParen);
    p.wrap(m, SyntaxKind::Parenthesized)
}

fn block(p: &mut Parser) {
    trace_fn!("parse_block");
    let m = p.marker();
    p.assert(SyntaxKind::LeftBrace);

    while !p.at(SyntaxKind::RightBrace) {
        code_expression(p);
    }

    p.expect_closing_delimiter(m, SyntaxKind::RightBrace);
    p.wrap(m, SyntaxKind::CodeBlock)
}
