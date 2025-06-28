use crate::ast::{AssignOp, BinOp};
use crate::kind::SyntaxKind;
use crate::parser::control_flow::{conditional, for_loop, while_loop};
use crate::parser::funcs::closure;
use crate::parser::{ExprContext, funcs, statements};
use crate::parser::{Marker, Parser};
use crate::precedence::{Precedence, PrecedenceTrait};
use crate::set::{SyntaxSet, UNARY_OP, syntax_set};
use crate::{Label, ast, set};
use compose_error_codes::{
    E0001_UNCLOSED_DELIMITER, E0002_INVALID_ASSIGNMENT, E0008_EXPECTED_EXPRESSION,
};
use compose_utils::{trace_fn, trace_log};
use ecow::eco_format;

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
            if Precedence::Call < min_prec {
                break;
            }
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
            if Precedence::Path < min_prec {
                break;
            }
            p.expect(SyntaxKind::Ident);
            p.wrap(m, SyntaxKind::PathAccess);
            continue;
        }

        // Handle field access `a.b.c`
        if p.eat_if(SyntaxKind::Dot) {
            if Precedence::Member < min_prec {
                break;
            }
            p.expect(SyntaxKind::Ident);
            p.wrap(m, SyntaxKind::FieldAccess);
            continue;
        }

        // Handle index access `a[2]`
        if p.eat_if(SyntaxKind::LeftBracket) {
            if Precedence::Index < min_prec {
                break;
            }
            code_expression(p);
            p.expect(SyntaxKind::RightBracket);
            p.wrap(m, SyntaxKind::IndexAccess);
            continue;
        }

        let bin_op = BinOp::from_kind(p.current());

        // Handle ambiguity between binary `|` op and closure with capture list
        if p.at(SyntaxKind::Pipe) {
            // tentatively check if this is a valid closure
            let checkpoint = p.checkpoint();
            if closure(p) {
                // If it was, this was not a binary op.
                // Restore and keep this expression atomic
                p.restore(checkpoint);
                break;
            } else {
                // if it wasn't a closure, restore and continue parsing as binary
                p.restore(checkpoint)
            }
        }

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

        if p.at_set(syntax_set!(Dots, DotsEq)) {
            if Precedence::Range < min_prec {
                break;
            }
            // handle range
            p.eat();
            if p.at_set(set::EXPR) {
                code_expression(p);
            }
            p.wrap(m, SyntaxKind::Range);
            continue;
        }

        break;
    }
}

fn err_assign_in_expr_context(p: &mut Parser) {
    debug_assert!(
        AssignOp::from_kind(p.current()).is_some(),
        "Not at an assignment"
    );
    let current = p.current();
    let op = current.descriptive_name();
    let is_eq = current == SyntaxKind::Eq;
    let lhs_text = p.last_text().to_owned();

    let error_marker = p.marker();

    p.insert_error_here("assignments are not allowed in expression contexts")
        .with_code(&E0002_INVALID_ASSIGNMENT)
        .with_label_message("assignment is not allowed here");
    p.eat(); // eat the AssignOp
    code_expression(p); // parse the rhs

    let rhs_text = p.last_text().to_owned();

    let err = p.err_at(error_marker).expect("An error was added");
    err.with_note(eco_format!(
        "assignments like `{lhs_text} {op} ...` are only valid as standalone statements"
    ));
    if is_eq {
        err.with_hint(eco_format!(
            "if you meant to compare `{lhs_text}` and `{rhs_text}`, use `==` instead of `=`"
        ));
        err.with_hint(eco_format!("if you meant to assign to `{lhs_text}`, wrap the statement in a block: `{{ {lhs_text} {op} ... }}`"));
        err.with_hint(eco_format!(
            "or introduce a new variable with `let`: `{{ let {lhs_text} = ... }}`"
        ));
    } else {
        err.with_hint(eco_format!(
            "to use `{op}` here, wrap the assignment in a block: `{{ {lhs_text} {op} ... }}`"
        ));
    }
}

/// Parse a primary expression.
///
/// A primary expressions are the building blocks in composable expressions.
fn primary_expr(p: &mut Parser, ctx: ExprContext) {
    trace_fn!("parse_primary_expr");
    let m = p.marker();
    match p.current() {
        // handle an ident that is not a closure
        SyntaxKind::Ident if ctx.is_atomic() || p.peek() != SyntaxKind::Arrow => {
            trace_fn!("parse_primary_expr: ident");
            p.eat();
        }
        // `_ = something`
        SyntaxKind::Underscore if !ctx.is_atomic() && p.peek() == SyntaxKind::Eq => {
            p.assert(SyntaxKind::Underscore);
            p.assert(SyntaxKind::Eq);
            code_expression(p);
            p.wrap(m, SyntaxKind::DestructureAssignment);
        }
        SyntaxKind::LeftBrace => block(p),
        SyntaxKind::LeftParen if at_unit_literal(p) => {
            trace_fn!("parse_primary_expr: unit literal");
            p.assert(SyntaxKind::LeftParen);
            p.assert(SyntaxKind::RightParen);
            p.wrap(m, SyntaxKind::Unit)
        }
        SyntaxKind::DotsEq | SyntaxKind::Dots => {
            p.eat();
            // for ..= an expression on the rhs is required
            if p.at(SyntaxKind::DotsEq) || p.at_set(set::EXPR) {
                code_expression(p);
            }
            p.wrap(m, SyntaxKind::Range);
        }
        SyntaxKind::LeftBracket => array(p),
        SyntaxKind::If => conditional(p),
        SyntaxKind::While => while_loop(p),
        SyntaxKind::For => for_loop(p),
        SyntaxKind::LeftParen => expr_with_parens(p, ctx),
        // Already fully handled in the lexer
        SyntaxKind::Int | SyntaxKind::Float | SyntaxKind::Bool | SyntaxKind::Str => p.eat(),
        SyntaxKind::Ref
        | SyntaxKind::Mut
        | SyntaxKind::Ident
        | SyntaxKind::Underscore
        | SyntaxKind::Pipe => {
            closure(p);
        }
        _ => err_expected_expression(
            p,
            Some(syntax_set!(
                NewLine,
                Semicolon,
                RightBrace,
                RightParen,
                RightBracket
            )),
        ),
    }
}

fn array(p: &mut Parser) {
    let m = p.marker();

    p.assert(SyntaxKind::LeftBracket);

    while !p.current().is_terminator() {
        if !p.at_set(set::EXPR) {
            p.unexpected("expected an expression in array list", None);
            continue;
        }

        code_expression(p);

        if !p.current().is_terminator() {
            p.expect_or_recover(SyntaxKind::Comma, syntax_set!(RightBracket));
        }
    }

    p.expect_closing_delimiter(m, SyntaxKind::RightBracket);

    p.wrap(m, SyntaxKind::Array)
}

fn block(p: &mut Parser) {
    trace_fn!("parse_block");
    let m = p.marker();
    p.assert(SyntaxKind::LeftBrace);

    statements::code(p, syntax_set!(End, RightBrace, RightParen, RightBracket));

    if !p.expect_closing_delimiter(m, SyntaxKind::RightBrace) {
        p.eat();
    }

    p.wrap(m, SyntaxKind::CodeBlock)
}

fn err_expected_expression(p: &mut Parser, recover_set: Option<SyntaxSet>) {
    p.unexpected("expected the start of an expression", recover_set)
        .with_code(&E0008_EXPECTED_EXPRESSION)
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
    if !parenthesized(p) || p.at(SyntaxKind::Arrow) {
        // if not, retry as a closure
        trace_fn!("parse_expr_with_parens: found closure");
        // It looks like it was a closure instead! Let's rewind and parse as a closure instead
        p.restore(checkpoint);
        closure(p);
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

fn parenthesized(p: &mut Parser) -> bool {
    trace_fn!("parse_parenthesized");
    let mut okay = true;
    let m = p.marker();
    p.assert(SyntaxKind::LeftParen);
    if p.at_set(set::EXPR) {
        code_expr_prec(p, ExprContext::Expr, Precedence::Lowest);
    } else if !p.at(SyntaxKind::RightParen) {
        err_expected_expression(p, None);
        okay = false;
    }

    if !p.expect_closing_delimiter(m, SyntaxKind::RightParen) {
        // If the closing parenthesis is missing or incorrect, try to recover
        p.recover_until(syntax_set!(RightParen, NewLine, Semicolon));
        p.eat_if(SyntaxKind::RightParen);
        okay = false;
    }
    p.wrap(m, SyntaxKind::Parenthesized);

    okay
}

pub(in crate::parser) fn err_unclosed_delim(
    p: &mut Parser,
    open_marker: Marker,
    expected_closing: SyntaxKind,
) {
    let closing_delim_label;
    if p.current().is_closing_delimiter() {
        closing_delim_label = Some(eco_format!(
            "unexpected closing delimiter `{}`",
            p.current().descriptive_name()
        ));
    } else if p.end() {
        closing_delim_label = None;
    } else {
        closing_delim_label = Some(eco_format!(
            "expected closing `{}`, but found `{}` instead",
            expected_closing.descriptive_name(),
            p.current_text()
        ));
    }
    let closing_span = p.current_span();
    p[open_marker]
        .convert_to_error("unclosed delimiter")
        .with_code(&E0001_UNCLOSED_DELIMITER)
        // label on the opening delimiter
        .with_label_message(eco_format!(
            "unclosed `{}` starts here",
            expected_closing
                .matching_delimiter()
                .unwrap()
                .descriptive_name()
        ))
        // trailing note
        .with_note(eco_format!(
            "expected `{}` to match the opening delimiter",
            expected_closing.descriptive_name()
        ));

    if let Some(closing_delim_label) = closing_delim_label {
        p.last_err()
            .expect("was just inserted")
            // label on (or near) the closing delimiter
            .with_label(Label::primary(closing_span, closing_delim_label));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_parse_tree;
    use crate::test_utils::*;
    use compose_error_codes::E0006_UNTERMINATED_STATEMENT;

    #[test]
    fn test_parse_parenthesized() {
        let input = r#"
            (1 + 2)
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::Parenthesized, |p| {
            p.assert_next(SyntaxKind::LeftParen, "(");
            p.assert_next_children(SyntaxKind::Binary, |p| {
                p.assert_next(SyntaxKind::Int, "1");
                p.assert_next(SyntaxKind::Plus, "+");
                p.assert_next(SyntaxKind::Int, "2");
                p.assert_end();
            });
            p.assert_next(SyntaxKind::RightParen, ")");
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_nested_parenthesized_and_closures() {
        let input = r#"
        ((a) => (a = (1 + 2), b = (c, d) => (c + d)) => a(b))
        "#;

        let mut p = assert_parse(input);
        p.assert_next_children(SyntaxKind::Parenthesized, |p| {
            p.assert_next(SyntaxKind::LeftParen, "(");
            p.assert_next_children(SyntaxKind::Closure, |p| {
                p.assert_next_children(SyntaxKind::Params, |p| {
                    p.assert_next(SyntaxKind::LeftParen, "(");
                    p.assert_next_children(SyntaxKind::Param, |p| {
                        p.assert_next(SyntaxKind::Ident, "a");
                        p.assert_end();
                    });
                    p.assert_next(SyntaxKind::RightParen, ")");
                    p.assert_end();
                });
                p.assert_next(SyntaxKind::Arrow, "=>");
                p.assert_next_children(SyntaxKind::Closure, |p| {
                    p.assert_next_children(SyntaxKind::Params, |p| {
                        p.assert_next(SyntaxKind::LeftParen, "(");
                        p.assert_next_children(SyntaxKind::Param, |p| {
                            p.assert_next_children(SyntaxKind::Named, |p| {
                                p.assert_next(SyntaxKind::Ident, "a");
                                p.assert_next(SyntaxKind::Eq, "=");
                                p.assert_next_children(SyntaxKind::Parenthesized, |p| {
                                    p.assert_next(SyntaxKind::LeftParen, "(");
                                    p.assert_next_children(SyntaxKind::Binary, |p| {
                                        p.assert_next(SyntaxKind::Int, "1");
                                        p.assert_next(SyntaxKind::Plus, "+");
                                        p.assert_next(SyntaxKind::Int, "2");
                                    });
                                    p.assert_next(SyntaxKind::RightParen, ")");
                                    p.assert_end();
                                });
                                p.assert_end();
                            });
                            p.assert_end();
                        });
                        p.assert_next(SyntaxKind::Comma, ",");
                        p.assert_next_children(SyntaxKind::Param, |p| {
                            p.assert_next_children(SyntaxKind::Named, |p| {
                                p.assert_next(SyntaxKind::Ident, "b");
                                p.assert_next(SyntaxKind::Eq, "=");
                                p.assert_next_children(SyntaxKind::Closure, |p| {
                                    p.assert_next_children(SyntaxKind::Params, |p| {
                                        p.assert_next(SyntaxKind::LeftParen, "(");
                                        p.assert_next_children(SyntaxKind::Param, |p| {
                                            p.assert_next(SyntaxKind::Ident, "c");
                                            p.assert_end();
                                        });
                                        p.assert_next(SyntaxKind::Comma, ",");
                                        p.assert_next_children(SyntaxKind::Param, |p| {
                                            p.assert_next(SyntaxKind::Ident, "d");
                                            p.assert_end();
                                        });
                                        p.assert_next(SyntaxKind::RightParen, ")");
                                        p.assert_end();
                                    });
                                    p.assert_next(SyntaxKind::Arrow, "=>");
                                    p.assert_next_children(SyntaxKind::Parenthesized, |p| {
                                        p.assert_next(SyntaxKind::LeftParen, "(");
                                        p.assert_next_children(SyntaxKind::Binary, |p| {
                                            p.assert_next(SyntaxKind::Ident, "c");
                                            p.assert_next(SyntaxKind::Plus, "+");
                                            p.assert_next(SyntaxKind::Ident, "d");
                                        });
                                        p.assert_next(SyntaxKind::RightParen, ")");
                                        p.assert_end();
                                    });
                                });
                            });
                            p.assert_end();
                        });
                        p.assert_next(SyntaxKind::RightParen, ")");
                        p.assert_end();
                    });
                    p.assert_next(SyntaxKind::Arrow, "=>");
                    p.assert_next_children(SyntaxKind::FuncCall, |p| {
                        p.assert_next(SyntaxKind::Ident, "a");
                        p.assert_next_children(SyntaxKind::Args, |p| {
                            p.assert_next(SyntaxKind::LeftParen, "(");
                            p.assert_next(SyntaxKind::Ident, "b");
                            p.assert_next(SyntaxKind::RightParen, ")");
                            p.assert_end();
                        });
                        p.assert_end();
                    });
                    p.assert_end();
                });
                p.assert_end();
            });
            p.assert_next(SyntaxKind::RightParen, ")");
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_more_parse_nested_parenthesized_and_closures() {
        let input = r#"
            (() => (a = v => println(v), c = (b = () => () => {}) => {}) => a(c))
        "#;

        let mut p = assert_parse(input);
        p.assert_next_children(SyntaxKind::Parenthesized, |p| {
            p.assert_next(SyntaxKind::LeftParen, "(");
            p.assert_next_children(SyntaxKind::Closure, |p| {
                p.assert_next_children(SyntaxKind::Params, |p| {
                    p.assert_next(SyntaxKind::LeftParen, "(");
                    p.assert_next(SyntaxKind::RightParen, ")");
                    p.assert_end();
                });
                p.assert_next(SyntaxKind::Arrow, "=>");
                p.assert_next_children(SyntaxKind::Closure, |p| {
                    p.assert_next_children(SyntaxKind::Params, |p| {
                        p.assert_next(SyntaxKind::LeftParen, "(");
                        p.assert_next_children(SyntaxKind::Param, |p| {
                            p.assert_next_children(SyntaxKind::Named, |p| {
                                p.assert_next(SyntaxKind::Ident, "a");
                                p.assert_next(SyntaxKind::Eq, "=");
                                p.assert_next_children(SyntaxKind::Closure, |p| {
                                    p.assert_next_children(SyntaxKind::Params, |p| {
                                        p.assert_next_children(SyntaxKind::Param, |p| {
                                            p.assert_next(SyntaxKind::Ident, "v");
                                            p.assert_end();
                                        });
                                        p.assert_end();
                                    });
                                    p.assert_next(SyntaxKind::Arrow, "=>");
                                    p.assert_next_children(SyntaxKind::FuncCall, |p| {
                                        p.assert_next(SyntaxKind::Ident, "println");
                                        p.assert_next_children(SyntaxKind::Args, |p| {
                                            p.assert_next(SyntaxKind::LeftParen, "(");
                                            p.assert_next(SyntaxKind::Ident, "v");
                                            p.assert_next(SyntaxKind::RightParen, ")");
                                            p.assert_end();
                                        });
                                        p.assert_end();
                                    });
                                });
                                p.assert_end();
                            });
                            p.assert_end();
                        });
                        p.assert_next(SyntaxKind::Comma, ",");
                        p.assert_next_children(SyntaxKind::Param, |p| {
                            p.assert_next_children(SyntaxKind::Named, |p| {
                                p.assert_next(SyntaxKind::Ident, "c");
                                p.assert_next(SyntaxKind::Eq, "=");
                                p.assert_next_children(SyntaxKind::Closure, |p| {
                                    p.assert_next_children(SyntaxKind::Params, |p| {
                                        p.assert_next(SyntaxKind::LeftParen, "(");
                                        p.assert_next_children(SyntaxKind::Param, |p| {
                                            p.assert_next_children(SyntaxKind::Named, |p| {
                                                p.assert_next(SyntaxKind::Ident, "b");
                                                p.assert_next(SyntaxKind::Eq, "=");
                                                p.assert_next_children(SyntaxKind::Closure, |p| {
                                                    p.assert_next_children(
                                                        SyntaxKind::Params,
                                                        |p| {
                                                            p.assert_next(
                                                                SyntaxKind::LeftParen,
                                                                "(",
                                                            );
                                                            p.assert_next(
                                                                SyntaxKind::RightParen,
                                                                ")",
                                                            );
                                                            p.assert_end();
                                                        },
                                                    );
                                                    p.assert_next(SyntaxKind::Arrow, "=>");
                                                    p.assert_next_children(
                                                        SyntaxKind::Closure,
                                                        |p| {
                                                            p.assert_next_children(
                                                                SyntaxKind::Params,
                                                                |p| {
                                                                    p.assert_next(
                                                                        SyntaxKind::LeftParen,
                                                                        "(",
                                                                    );
                                                                    p.assert_next(
                                                                        SyntaxKind::RightParen,
                                                                        ")",
                                                                    );
                                                                },
                                                            );
                                                            p.assert_next(SyntaxKind::Arrow, "=>");
                                                            p.assert_next_children(
                                                                SyntaxKind::CodeBlock,
                                                                |p| {
                                                                    p.assert_next(
                                                                        SyntaxKind::LeftBrace,
                                                                        "{",
                                                                    );
                                                                    p.assert_next(
                                                                        SyntaxKind::RightBrace,
                                                                        "}",
                                                                    );
                                                                    p.assert_end();
                                                                },
                                                            );
                                                            p.assert_end();
                                                        },
                                                    );
                                                    p.assert_end();
                                                });
                                                p.assert_end();
                                            });
                                            p.assert_end();
                                        });
                                        p.assert_next(SyntaxKind::RightParen, ")");
                                        p.assert_end();
                                    });
                                    p.assert_next(SyntaxKind::Arrow, "=>");
                                    p.assert_next_children(SyntaxKind::CodeBlock, |p| {
                                        p.assert_next(SyntaxKind::LeftBrace, "{");
                                        p.assert_next(SyntaxKind::RightBrace, "}");
                                    });
                                    p.assert_end();
                                });
                                p.assert_end();
                            });
                            p.assert_end();
                        });
                        p.assert_next(SyntaxKind::RightParen, ")");
                        p.assert_end();
                    });
                    p.assert_next(SyntaxKind::Arrow, "=>");
                    p.assert_next_children(SyntaxKind::FuncCall, |p| {
                        p.assert_next(SyntaxKind::Ident, "a");
                        p.assert_next_children(SyntaxKind::Args, |p| {
                            p.assert_next(SyntaxKind::LeftParen, "(");
                            p.assert_next(SyntaxKind::Ident, "c");
                            p.assert_next(SyntaxKind::RightParen, ")");
                            p.assert_end();
                        });
                        p.assert_end();
                    });
                    p.assert_end();
                });
                p.assert_end();
            });
            p.assert_next(SyntaxKind::RightParen, ")");
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_unit_literal() {
        let input = r#"
            ()
        "#;

        let mut p = assert_parse(input);
        p.assert_next_children(SyntaxKind::Unit, |p| {
            p.assert_next(SyntaxKind::LeftParen, "(");
            p.assert_next(SyntaxKind::RightParen, ")");
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_code_block() {
        let input = r#"
            {
                println("hello");
                do_other_stuff();
            }
        "#;

        let mut p = assert_parse(input);
        p.assert_next_children(SyntaxKind::CodeBlock, |p| {
            p.assert_next(SyntaxKind::LeftBrace, "{");
            p.assert_next_children(SyntaxKind::FuncCall, |_| {});
            p.assert_next_children(SyntaxKind::FuncCall, |_| {});
            p.assert_next(SyntaxKind::RightBrace, "}");
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_code_block_incorrect_closing() {
        let input = r#"
            {
                println("hello");
                do_other_stuff();
            );
            let a = 4;
        "#;

        let mut p = assert_parse_with_errors(input, &[E0001_UNCLOSED_DELIMITER]);
        p.assert_next_children(SyntaxKind::CodeBlock, |p| {
            p.assert_next_error(E0001_UNCLOSED_DELIMITER); // unclosed `{`
            p.assert_next_children(SyntaxKind::FuncCall, |_| {});
            p.assert_next_children(SyntaxKind::FuncCall, |_| {});
            p.assert_next(SyntaxKind::RightParen, ")");
            p.assert_end();
        });

        p.assert_next_children(SyntaxKind::LetBinding, |_| {});
        p.assert_end();
    }

    #[test]
    fn test_parse_assignment_in_expression_context() {
        assert_parse_tree!(
            "let inc = v => v += 1;",
            LetBinding [
                Let("let")
                Ident("inc")
                Eq("=")
                Closure [
                    Params [
                        Param [
                            Ident("v")
                        ]
                    ]
                    Arrow("=>")
                    Ident("v")
                    Error(E0002_INVALID_ASSIGNMENT)
                    PlusEq("+=")
                    Int("1")
                ]
            ]
        );
    }

    #[test]
    fn test_disambiguate_closure_and_pipe() {
        let input = r#"
            a | a | a | a
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::Binary, |p| {
            p.assert_next(SyntaxKind::Ident, "a");
            p.assert_next(SyntaxKind::Pipe, "|");
            p.assert_next_children(SyntaxKind::Binary, |p| {
                p.assert_next(SyntaxKind::Ident, "a");
                p.assert_next(SyntaxKind::Pipe, "|");
                p.assert_next_children(SyntaxKind::Binary, |p| {
                    p.assert_next(SyntaxKind::Ident, "a");
                    p.assert_next(SyntaxKind::Pipe, "|");
                    p.assert_next(SyntaxKind::Ident, "a");
                    p.assert_end();
                });
                p.assert_end();
            });
            p.assert_end();
        });
        p.assert_end();

        let input = r#"
            a | a | () => {}
        "#;

        let mut p = assert_parse_with_errors(input, &[E0006_UNTERMINATED_STATEMENT]);

        p.assert_next(SyntaxKind::Ident, "a");
        // Should be an error as the end of the statement would be expected here
        // `a` and `| a | () => {}` are separate expression statements
        p.assert_next_error(E0006_UNTERMINATED_STATEMENT);
        p.assert_next_children(SyntaxKind::Closure, |p| {
            p.assert_next_children(SyntaxKind::CaptureList, |p| {
                p.assert_next(SyntaxKind::Pipe, "|");
                p.assert_next_children(SyntaxKind::Capture, |p| {
                    p.assert_next(SyntaxKind::Ident, "a");
                    p.assert_end()
                });
                p.assert_next(SyntaxKind::Pipe, "|");
                p.assert_end();
            });
            p.assert_next_children(SyntaxKind::Params, |p| {
                p.assert_next(SyntaxKind::LeftParen, "(");
                p.assert_next(SyntaxKind::RightParen, ")");
                p.assert_end();
            });
            p.assert_next(SyntaxKind::Arrow, "=>");
            p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn parse_array() {
        let input = r#"
            [1, 2, 3]
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::Array, |p| {
            p.assert_next(SyntaxKind::LeftBracket, "[");
            p.assert_next(SyntaxKind::Int, "1");
            p.assert_next(SyntaxKind::Comma, ",");
            p.assert_next(SyntaxKind::Int, "2");
            p.assert_next(SyntaxKind::Comma, ",");
            p.assert_next(SyntaxKind::Int, "3");
            p.assert_next(SyntaxKind::RightBracket, "]");
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn parse_range_full() {
        assert_parse("..")
            .assert_next_children(SyntaxKind::Range, |p| {
                p.assert_next(SyntaxKind::Dots, "..");
                p.assert_end();
            })
            .assert_end();
    }

    #[test]
    fn parse_range_to_inclusive() {
        assert_parse("..=4")
            .assert_next_children(SyntaxKind::Range, |p| {
                p.assert_next(SyntaxKind::DotsEq, "..=");
                p.assert_next(SyntaxKind::Int, "4");
                p.assert_end();
            })
            .assert_end();
    }

    #[test]
    fn parse_range_to_exclusive() {
        assert_parse("..4")
            .assert_next_children(SyntaxKind::Range, |p| {
                p.assert_next(SyntaxKind::Dots, "..");
                p.assert_next(SyntaxKind::Int, "4");
                p.assert_end();
            })
            .assert_end();
    }

    #[test]
    fn parse_range_from() {
        assert_parse("4..")
            .assert_next_children(SyntaxKind::Range, |p| {
                p.assert_next(SyntaxKind::Int, "4");
                p.assert_next(SyntaxKind::Dots, "..");
                p.assert_end();
            })
            .assert_end();
    }

    #[test]
    fn parse_range_from_to_inclusive() {
        assert_parse("4..=8")
            .assert_next_children(SyntaxKind::Range, |p| {
                p.assert_next(SyntaxKind::Int, "4");
                p.assert_next(SyntaxKind::DotsEq, "..=");
                p.assert_next(SyntaxKind::Int, "8");
                p.assert_end();
            })
            .assert_end();
    }

    #[test]
    fn parse_range_from_to_exclusive() {
        assert_parse("4..8")
            .assert_next_children(SyntaxKind::Range, |p| {
                p.assert_next(SyntaxKind::Int, "4");
                p.assert_next(SyntaxKind::Dots, "..");
                p.assert_next(SyntaxKind::Int, "8");
            })
            .assert_end();
    }

    #[test]
    fn parse_range_in_the_wild() {
        assert_parse_tree!(
            "foo(..2)",
            FuncCall [
                Ident("foo")
                Args [
                    LeftParen("(")
                    Range [
                        Dots("..")
                        Int("2")
                    ]
                    RightParen(")")
                ]
            ]
        );
        
        assert_parse_tree!(
            "a[2..]",
            IndexAccess [
                Ident("a")
                LeftBracket("[")
                Range [
                    Int("2")
                    Dots("..")
                ]
                RightBracket("]")
            ]
        );
        assert_parse_tree!(
            "b[..=2]",
            IndexAccess [
                Ident("b")
                LeftBracket("[")
                Range [
                    DotsEq("..=")
                    Int("2")
                ]
                RightBracket("]")
            ]
        );
    }

    #[test]
    fn test_parse_range_with_expressions() {
        assert_parse_tree!(
            "(1 + 2)..(3 + 4)",
            Range [
                Parenthesized [
                    LeftParen("(")
                    Binary [
                        Int("1")
                        Plus("+")
                        Int("2")
                    ]
                    RightParen(")")
                ]
                Dots("..")
                Parenthesized [
                    LeftParen("(")
                    Binary [
                        Int("3")
                        Plus("+")
                        Int("4")
                    ]
                    RightParen(")")
                ]
            ]
        );
    }

    #[test]
    fn test_parse_range_with_function_calls() {
        assert_parse_tree!(
            "min()..max()",
            Range [
                FuncCall [
                    Ident("min")
                    Args [
                        LeftParen("(")
                        RightParen(")")
                    ]
                ]
                Dots("..")
                FuncCall [
                    Ident("max")
                    Args [
                        LeftParen("(")
                        RightParen(")")
                    ]
                ]
            ]
        );
    }

    #[test]
    fn test_parse_range_with_variables() {
        assert_parse_tree!(
            "start..=end",
            Range [
                Ident("start")
                DotsEq("..=")
                Ident("end")
            ]
        );
    }

    #[test]
    fn test_parse_nested_ranges() {
        assert_parse_tree!(
            "a..=b..=c",
            Range [
                Ident("a")
                DotsEq("..=")
                Range [
                    Ident("b") DotsEq("..=") Ident("c")
                ]
            ]
        );
    }

    #[test]
    fn test_parse_range_with_field_access() {
        assert_parse_tree!(
            "obj.start..obj.end",
            Range [
                FieldAccess [
                    Ident("obj")
                    Dot(".")
                    Ident("start")
                ]
                Dots("..")
                FieldAccess [
                    Ident("obj")
                    Dot(".")
                    Ident("end")
                ]
            ]
        );
    }

    #[test]
    fn test_parse_range_with_method_calls() {
        assert_parse_tree!(
            "obj.start()..obj.end()",
            Range [
                FuncCall [
                    FieldAccess [
                        Ident("obj")
                        Dot(".")
                        Ident("start")
                    ]
                    Args [
                        LeftParen("(")
                        RightParen(")")
                    ]
                ]
                Dots("..")
                FuncCall [
                    FieldAccess [
                        Ident("obj")
                        Dot(".")
                        Ident("end")
                    ]
                    Args [
                        LeftParen("(")
                        RightParen(")")
                    ]
                ]
            ]
        );
    }

    #[test]
    fn test_parse_range_with_arithmetic_operators() {
        // Range should bind looser than arithmetic operators
        assert_parse_tree!(
            "a + b..c",
            Range [
                Binary [
                    Ident("a")
                    Plus("+")
                    Ident("b")
                ]
                Dots("..")
                Ident("c")
            ]
        );
    }

    #[test]
    fn test_path_access_precedence() {
        // Path access should bind tighter than calls
        assert_parse_tree!(
            "b::c()",
            FuncCall [
                PathAccess [
                    Ident("b")
                    ColonColon("::")
                    Ident("c")
                ]
                Args [
                    LeftParen("(")
                    RightParen(")")
                ]
            ]
        );
    }

    #[test]
    fn test_mixed_access_types() {
        assert_parse_tree!(
            "a.b::c[d].e",
            FieldAccess [
                IndexAccess [
                    PathAccess [
                        FieldAccess [
                            Ident("a")
                            Dot(".")
                            Ident("b")
                        ]
                        ColonColon("::")
                        Ident("c")
                    ]
                    LeftBracket("[")
                    Ident("d")
                    RightBracket("]")
                ]
                Dot(".")
                Ident("e")
            ]
        );
    }

    #[test]
    fn test_parse_complex_chain() {
        assert_parse_tree!(
            "a.b().c::d().e[f]()",
            FuncCall [
                IndexAccess [
                    FieldAccess [
                        FuncCall [
                            PathAccess [
                                FieldAccess [
                                    FuncCall [
                                        FieldAccess [
                                            Ident("a")
                                            Dot(".")
                                            Ident("b")
                                        ]
                                        Args [
                                            LeftParen("(")
                                            RightParen(")")
                                        ]
                                    ]
                                    Dot(".")
                                    Ident("c")
                                ]
                                ColonColon("::")
                                Ident("d")
                            ]
                            Args [
                                LeftParen("(")
                                RightParen(")")
                            ]
                        ]
                        Dot(".")
                        Ident("e")
                    ]
                    LeftBracket("[")
                    Ident("f")
                    RightBracket("]")
                ]
                Args [
                    LeftParen("(")
                    RightParen(")")
                ]
            ]
        );
    }

    #[test]
    fn test_nested_indexing_with_expressions() {
        // Test complex index expressions
        assert_parse_tree!(
            "a[b.c[d::e]].f",
            FieldAccess [
                IndexAccess [
                    Ident("a")
                    LeftBracket("[")
                    IndexAccess [
                        FieldAccess [
                            Ident("b")
                            Dot(".")
                            Ident("c")
                        ]
                        LeftBracket("[")
                        PathAccess [
                            Ident("d")
                            ColonColon("::")
                            Ident("e")
                        ]
                        RightBracket("]")
                    ]
                    RightBracket("]")
                ]
                Dot(".")
                Ident("f")
            ]
        );
    }

    #[test]
    fn test_call_with_complex_arguments() {
        // Test function calls with complex argument expressions
        assert_parse_tree!(
            "a.b(c::d.e[f], g.h())",
            FuncCall [
                FieldAccess [ Ident("a") Dot(".") Ident("b") ]
                Args [
                    LeftParen("(")
                    IndexAccess [
                        FieldAccess [
                            PathAccess [ Ident("c") ColonColon("::") Ident("d") ]
                            Dot(".")
                            Ident("e")
                        ]
                        LeftBracket("[") Ident("f") RightBracket("]")
                    ]
                    Comma(",")
                    FuncCall [
                        FieldAccess [ Ident("g") Dot(".") Ident("h") ]
                        Args [ LeftParen("(") RightParen(")") ]
                    ]
                    RightParen(")")
                ]
            ]
        );
    }
}
