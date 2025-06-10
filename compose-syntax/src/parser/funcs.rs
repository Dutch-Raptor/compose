use crate::kind::SyntaxKind;
use crate::parser::{expressions, patterns};
use crate::parser::{ExprContext, Parser};
use crate::precedence::Precedence;
use crate::set;
use crate::set::{syntax_set, ARG_RECOVER};
use compose_error_codes::E0009_ARGS_MISSING_COMMAS;
use compose_utils::trace_fn;
use std::collections::HashSet;

pub fn args(p: &mut Parser) {
    trace_fn!("parse_args");
    let m = p.marker();
    p.expect(SyntaxKind::LeftParen);

    while !p.current().is_terminator() {
        arg(p);

        if !p.current().is_terminator() && !p.eat_if(SyntaxKind::Comma) {
            p.insert_error_before("expected a comma between the function arguments")
                .with_code(&E0009_ARGS_MISSING_COMMAS)
                .with_label_message("help: insert a comma here");
        }
    }

    p.expect_closing_delimiter(m, SyntaxKind::RightParen);

    p.wrap(m, SyntaxKind::Args);
}

fn arg(p: &mut Parser) {
    let m_mods = p.marker();

    let mut had_modifiers = false;
    had_modifiers |= p.eat_if(SyntaxKind::Ref);
    had_modifiers |= p.eat_if(SyntaxKind::Mut);

    let m = p.marker();
    expressions::code_expression(p);

    if p.eat_if(SyntaxKind::Colon) {
        if p[m].kind() != SyntaxKind::Ident {
            p[m].expected("identifier");
        }

        if had_modifiers {
            p[m_mods].convert_to_error("argument modifiers like `ref` and `mut` go before the expression in named arguments")
            .with_label_message("help: move the modifiers to the right of the `=`");
        }

        expressions::code_expression(p);
        p.wrap(m, SyntaxKind::Named)
    }
}

pub fn closure(p: &mut Parser) -> bool {
    trace_fn!("parse_closure");
    let mut okay = true;
    let m = p.marker();

    if p.at(SyntaxKind::Pipe) {
        okay &= captures(p);
    }

    okay &= params(p);

    okay &= p
        .expect_or_recover_until(
            SyntaxKind::Arrow,
            "expected `=>` after closure parameters",
            syntax_set!(Arrow, LeftBrace, NewLine),
        )
        .map(|e| {
            e.with_label_message("help: you probably meant to write `=>` here");
        })
        .is_ok();

    expressions::code_expression(p);

    p.wrap(m, SyntaxKind::Closure);

    okay
}

fn captures(p: &mut Parser) -> bool {
    debug_assert!(p.at(SyntaxKind::Pipe), "Expected `|`");
    let mut okay = true;
    let m = p.marker();
    p.assert(SyntaxKind::Pipe);

    while !p.at_set(syntax_set!(Pipe, End)) {
        if !p.at_set(set::CAPTURE) {
            okay = false;
            p.unexpected("expected a capture declaration", None);
            continue;
        }

        capture(p);

        if !p.at_set(syntax_set!(Pipe, End)) {
            okay &= p.expect(SyntaxKind::Comma)
        }
    }

    okay &= p.expect_closing_delimiter(m, SyntaxKind::Pipe);

    p.wrap(m, SyntaxKind::CaptureList);

    okay
}

fn capture(p: &mut Parser) {
    let m = p.marker();

    p.eat_if(SyntaxKind::Ref);
    p.eat_if(SyntaxKind::Mut);

    let ident_m = p.marker();
    // Parse a full expression, even though we only care about an identifier.
    // This way the entire expression can be marked as an error if it is not.
    expressions::code_expr_prec(p, ExprContext::AtomicExpr, Precedence::Lowest);

    if p[ident_m].kind() != SyntaxKind::Ident {
        p.expected("an identifier");
    }

    p.wrap(m, SyntaxKind::Capture)
}

/// Parse closure parameters.
pub fn params(p: &mut Parser) -> bool {
    let m = p.marker();
    let mut okay = true;
    let wrapped_in_parens = p.eat_if(SyntaxKind::LeftParen);

    let mut seen = HashSet::new();

    if !wrapped_in_parens {
        // Only allow one param if not wrapped
        param(p, &mut seen);
    } else {
        while !p.current().is_terminator() {
            if !p.at_set(set::PARAM) {
                p.unexpected("expected a param", None);
                okay = false;
                continue;
            }

            param(p, &mut seen);

            if !p.current().is_terminator() {
                okay &= p.expect_or_recover(SyntaxKind::Comma, ARG_RECOVER);
            }
        }
    }

    if !wrapped_in_parens && p.at(SyntaxKind::Comma) {
        p.insert_error_here("to have multiple parameters, wrap them in parentheses");
        p.recover_until(syntax_set!(RightParen, Arrow));
        okay = false;
    }

    if wrapped_in_parens {
        okay &= p.expect_closing_delimiter(m, SyntaxKind::RightParen);
    }
    p.wrap(m, SyntaxKind::Params);

    okay
}

fn param<'s>(p: &mut Parser<'s>, seen: &mut HashSet<&'s str>) {
    trace_fn!("parse_param");
    let m = p.marker();

    p.eat_if(SyntaxKind::Ref);
    p.eat_if(SyntaxKind::Mut);

    let was_at_pat = p.at_set(set::PATTERN);
    let pat_m = p.marker();

    patterns::pattern(p, false, seen, Some("parameter"));

    // Parse named params like `a = 1`
    if p.eat_if(SyntaxKind::Eq) {
        if was_at_pat && p[pat_m].kind() != SyntaxKind::Ident {
            p[m].expected("identifier");
        }

        expressions::code_expression(p);
        p.wrap(pat_m, SyntaxKind::Named)
    }

    p.wrap(m, SyntaxKind::Param)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use compose_error_codes::E0001_UNCLOSED_DELIMITER;

    #[test]
    fn test_parse_closure_multiple_params() {
        let input = r#"
            (a, b, c) => {};
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::Closure, |p| {
            p.assert_next_children(SyntaxKind::Params, |p| {
                p.assert_next(SyntaxKind::LeftParen, "(");
                p.assert_next_children(SyntaxKind::Param, |p| {
                    p.assert_next(SyntaxKind::Ident, "a");
                });
                p.assert_next(SyntaxKind::Comma, ",");
                p.assert_next_children(SyntaxKind::Param, |p| {
                    p.assert_next(SyntaxKind::Ident, "b");
                });
                p.assert_next(SyntaxKind::Comma, ",");
                p.assert_next_children(SyntaxKind::Param, |p| {
                    p.assert_next(SyntaxKind::Ident, "c");
                });
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
    fn test_parse_closure_single_param() {
        let input = r#"
            (a) => {};
        "#;

        let mut p = assert_parse(input);

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
            p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_closure_single_param_no_parens() {
        let input = r#"
            a => {};
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::Closure, |p| {
            p.assert_next_children(SyntaxKind::Params, |p| {
                p.assert_next_children(SyntaxKind::Param, |p| {
                    p.assert_next(SyntaxKind::Ident, "a");
                    p.assert_end();
                });
                p.assert_end();
            });
            p.assert_next(SyntaxKind::Arrow, "=>");
            p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_closure_discard_param() {
        let input = r#"
            _ => {};
        "#;

        let mut p = assert_parse(input);
        p.assert_next_children(SyntaxKind::Closure, |p| {
            p.assert_next_children(SyntaxKind::Params, |p| {
                p.assert_next_children(SyntaxKind::Param, |p| {
                    p.assert_next(SyntaxKind::Underscore, "_");
                    p.assert_end();
                });
                p.assert_end();
            });
            p.assert_next(SyntaxKind::Arrow, "=>");
            p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_closure_named_param() {
        let input = r#"
            (a = b) => {};
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::Closure, |p| {
            p.assert_next_children(SyntaxKind::Params, |p| {
                p.assert_next(SyntaxKind::LeftParen, "(");
                p.assert_next_children(SyntaxKind::Param, |p| {
                    p.assert_next_children(SyntaxKind::Named, |p| {
                        p.assert_next(SyntaxKind::Ident, "a");
                        p.assert_next(SyntaxKind::Eq, "=");
                        p.assert_next(SyntaxKind::Ident, "b");
                    });
                    p.assert_end();
                });
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
    fn test_parse_args() {
        let input = r#"
            f(a, b, c)
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::FuncCall, |p| {
            p.assert_next(SyntaxKind::Ident, "f");
            p.assert_next_children(SyntaxKind::Args, |p| {
                p.assert_next(SyntaxKind::LeftParen, "(");
                p.assert_next(SyntaxKind::Ident, "a");
                p.assert_next(SyntaxKind::Comma, ",");
                p.assert_next(SyntaxKind::Ident, "b");
                p.assert_next(SyntaxKind::Comma, ",");
                p.assert_next(SyntaxKind::Ident, "c");
                p.assert_next(SyntaxKind::RightParen, ")");
                p.assert_end();
            });
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_args_empty() {
        let input = r#"
            f()
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::FuncCall, |p| {
            p.assert_next(SyntaxKind::Ident, "f");
            p.assert_next_children(SyntaxKind::Args, |p| {
                p.assert_next(SyntaxKind::LeftParen, "(");
                p.assert_next(SyntaxKind::RightParen, ")");
                p.assert_end();
            });
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_args_named() {
        let input = r#"
            f(a: b)
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::FuncCall, |p| {
            p.assert_next(SyntaxKind::Ident, "f");
            p.assert_next_children(SyntaxKind::Args, |p| {
                p.assert_next(SyntaxKind::LeftParen, "(");
                p.assert_next_children(SyntaxKind::Named, |p| {
                    p.assert_next(SyntaxKind::Ident, "a");
                    p.assert_next(SyntaxKind::Colon, ":");
                    p.assert_next(SyntaxKind::Ident, "b");
                    p.assert_end();
                });
                p.assert_next(SyntaxKind::RightParen, ")");
                p.assert_end();
            });
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_args_named_and_positional() {
        let input = r#"
            f(a: b, c)
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::FuncCall, |p| {
            p.assert_next(SyntaxKind::Ident, "f");
            p.assert_next_children(SyntaxKind::Args, |p| {
                p.assert_next(SyntaxKind::LeftParen, "(");
                p.assert_next_children(SyntaxKind::Named, |p| {
                    p.assert_next(SyntaxKind::Ident, "a");
                    p.assert_next(SyntaxKind::Colon, ":");
                    p.assert_next(SyntaxKind::Ident, "b");
                    p.assert_end();
                });
                p.assert_next(SyntaxKind::Comma, ",");
                p.assert_next(SyntaxKind::Ident, "c");
                p.assert_next(SyntaxKind::RightParen, ")");
                p.assert_end();
            });
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_args_malformed() {
        let input = r#"
            f(a, b, c
            1 + 2
        "#;

        let mut p = assert_parse_with_errors(
            input,
            &[E0001_UNCLOSED_DELIMITER, E0009_ARGS_MISSING_COMMAS],
        );

        p.assert_next_children(SyntaxKind::FuncCall, |p| {
            p.assert_next(SyntaxKind::Ident, "f");
            p.assert_next_children(SyntaxKind::Args, |p| {
                p.assert_next_error(E0001_UNCLOSED_DELIMITER);
                p.assert_next(SyntaxKind::Ident, "a");
                p.assert_next(SyntaxKind::Comma, ",");
                p.assert_next(SyntaxKind::Ident, "b");
                p.assert_next(SyntaxKind::Comma, ",");
                p.assert_next(SyntaxKind::Ident, "c");
                p.assert_next_error(E0009_ARGS_MISSING_COMMAS);
                p.assert_next_children(SyntaxKind::Binary, |p| {
                    p.assert_next(SyntaxKind::Int, "1");
                    p.assert_next(SyntaxKind::Plus, "+");
                    p.assert_next(SyntaxKind::Int, "2");
                    p.assert_end();
                });
                p.assert_end();
            });
            p.assert_end();
        });

        p.assert_end();
    }

    #[test]
    fn test_parse_params_ref_mut_combinations() {
        for (ref_, mut_) in [(true, false), (false, true), (true, true), (false, false)] {
            let input = format!(
                r#"
                    ({ref} {mut} a) => {{}}
                "#,
                ref = if ref_ { "ref " } else { "" },
                mut = if mut_ { "mut " } else { "" }
            );

            let mut p = assert_parse(&input);
            p.assert_next_children(SyntaxKind::Closure, |p| {
                p.assert_next_children(SyntaxKind::Params, |p| {
                    p.assert_next(SyntaxKind::LeftParen, "(");
                    p.assert_next_children(SyntaxKind::Param, |p| {
                        if ref_ {
                            p.assert_next(SyntaxKind::Ref, "ref");
                        }
                        if mut_ {
                            p.assert_next(SyntaxKind::Mut, "mut");
                        }
                        p.assert_next(SyntaxKind::Ident, "a");
                        p.assert_end();
                    });
                    p.assert_next(SyntaxKind::RightParen, ")");
                    p.assert_end();
                });
            });
            p.assert_end();
        }
    }

    #[test]
    fn test_parse_closure_with_capture_list() {
        let input = r#"
            |a, ref b, mut c, ref mut d| (e) => {}
        "#;

        let mut p = assert_parse(input);

        p.assert_next_children(SyntaxKind::Closure, |p| {
            p.assert_next_children(SyntaxKind::CaptureList, |p| {
                p.assert_next(SyntaxKind::Pipe, "|");
                p.assert_next_children(SyntaxKind::Capture, |p| {
                    p.assert_next(SyntaxKind::Ident, "a");
                    p.assert_end();
                });
                p.assert_next(SyntaxKind::Comma, ",");
                p.assert_next_children(SyntaxKind::Capture, |p| {
                    p.assert_next(SyntaxKind::Ref, "ref");
                    p.assert_next(SyntaxKind::Ident, "b");
                    p.assert_end();
                });
                p.assert_next(SyntaxKind::Comma, ",");
                p.assert_next_children(SyntaxKind::Capture, |p| {
                    p.assert_next(SyntaxKind::Mut, "mut");
                    p.assert_next(SyntaxKind::Ident, "c");
                    p.assert_end();
                });
                p.assert_next(SyntaxKind::Comma, ",");
                p.assert_next_children(SyntaxKind::Capture, |p| {
                    p.assert_next(SyntaxKind::Ref, "ref");
                    p.assert_next(SyntaxKind::Mut, "mut");
                    p.assert_next(SyntaxKind::Ident, "d");
                    p.assert_end();
                });
                p.assert_next(SyntaxKind::Pipe, "|");
                p.assert_end();
            });
            p.assert_next_children(SyntaxKind::Params, |p| {
                p.assert_next(SyntaxKind::LeftParen, "(");
                p.assert_next_children(SyntaxKind::Param, |p| {
                    p.assert_next(SyntaxKind::Ident, "e");
                    p.assert_end();
                });
            });
            p.assert_next(SyntaxKind::Arrow, "=>");
            p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
            p.assert_end();
        });
        p.assert_end();
    }
}
