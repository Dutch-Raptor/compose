use crate::kind::SyntaxKind;
use crate::node::SyntaxErrorSeverity;
use crate::parser::Parser;
use crate::parser::expressions::code_expression;
use crate::parser::patterns::pattern;
use crate::parser::statements::code;
use crate::set::syntax_set;
use crate::{Label, Span, SyntaxError, SyntaxNode, set};
use compose_error_codes::{E0005_IF_EXPRESSION_BODIES_REQUIRE_BRACES, W0002_UNNECESSARY_PARENTHESES_AROUND_CONDITION, W0003_UNNECESSARY_PARENTHESES_IN_FOR_EXPRESSION};
use compose_utils::trace_fn;
use std::collections::HashSet;

pub(crate) fn conditional(p: &mut Parser) {
    trace_fn!("parse_conditional");
    let m = p.marker();
    p.assert(SyntaxKind::If);

    condition(p);

    if !parse_control_flow_block(p, ControlFlow::If) {
        p.wrap(m, SyntaxKind::Conditional);
        return;
    }

    while p.at(SyntaxKind::Else) {
        trace_fn!("parse_else_maybe_if");
        let else_marker = p.marker();
        p.assert(SyntaxKind::Else);
        if p.eat_if(SyntaxKind::If) {
            trace_fn!("parse_else_if");
            condition(p);

            if !parse_control_flow_block(p, ControlFlow::If) {
                p.wrap(m, SyntaxKind::Conditional);
                return;
            }
            p.wrap(else_marker, SyntaxKind::ConditionalAlternate);
            continue;
        } else {
            if !parse_control_flow_block(p, ControlFlow::Else) {
                p.wrap(m, SyntaxKind::Conditional);
                return;
            }

            p.wrap(else_marker, SyntaxKind::ConditionalElse);
        }
    }

    p.wrap(m, SyntaxKind::Conditional);
}

pub fn while_loop(p: &mut Parser) {
    trace_fn!("parse_while_loop");
    let m = p.marker();
    p.assert(SyntaxKind::While);

    condition(p);

    parse_control_flow_block(p, ControlFlow::While);
    p.wrap(m, SyntaxKind::WhileLoop);
}

pub fn for_loop(p: &mut Parser) {
    trace_fn!("parse_for_loop");
    let m = p.marker();
    p.assert(SyntaxKind::For);

    let mut wrapped = false;
    let left_paren_marker = p.marker();
    if p.at(SyntaxKind::LeftParen) {
        let checkpoint = p.checkpoint();

        // Attempt to parse as pattern (the pattern may start with `(`)
        pattern(p, true, &mut HashSet::new(), None);

        if !p.at(SyntaxKind::In) {
            // Turns out it was not just a pattern. Maybe the loop pattern is wrapped in parens?
            p.restore(checkpoint);
            wrapped = true;
            p.assert(SyntaxKind::LeftParen);
            pattern(p, false, &mut HashSet::new(), None);
        }
    } else {
        pattern(p, false, &mut HashSet::new(), None);
    }

    p.expect(SyntaxKind::In);

    // parse the iterable
    code_expression(p);

    if wrapped {
        if p.expect_closing_delimiter(left_paren_marker, SyntaxKind::RightParen) {
            let open = p[left_paren_marker].span();
            let close = p
                .last_node()
                .map(SyntaxNode::span)
                .unwrap_or(Span::detached());
            
            p.insert_error(SyntaxError::new(
                "unnecessary parentheses in `for` expression",
                open,
            ))
            .with_severity(SyntaxErrorSeverity::Warning)
            .with_code(&W0003_UNNECESSARY_PARENTHESES_IN_FOR_EXPRESSION)
            .with_label(Label::primary(close, "help: remove these parentheses"));
        }
    }

    parse_control_flow_block(p, ControlFlow::For);

    p.wrap(m, SyntaxKind::ForLoop);
}

fn condition(p: &mut Parser) {
    trace_fn!("parse_condition");
    let cond_marker = p.marker();
    code_expression(p);
    let last_node = p.last_node().unwrap();
    if last_node.kind() == SyntaxKind::Parenthesized {
        p.insert_error(SyntaxError::new(
            "unnecessary parentheses around condition",
            last_node.span(),
        ))
        .with_severity(SyntaxErrorSeverity::Warning)
        .with_code(&W0002_UNNECESSARY_PARENTHESES_AROUND_CONDITION)
        .with_label_message("help: remove these parentheses");
    }
    p.wrap(cond_marker, SyntaxKind::Condition);
}

#[derive(Debug, Clone, Copy)]
enum ControlFlow {
    If,
    While,
    For,
    Else,
}

fn parse_control_flow_block(p: &mut Parser, flow: ControlFlow) -> bool {
    trace_fn!("parse_control_flow_block");
    let open_delim = p.marker();
    let had_open_brace = p.at(SyntaxKind::LeftBrace);
    if !p.eat_if(SyntaxKind::LeftBrace) {
        err_missing_braces(p, flow);

        if !p.at_set(set::STMT) {
            // the left brace isnt missing, something very unexpected is happening
            // recover until the end of the entire statement if possible
            // then give up
            p.recover_until(syntax_set!(RightBrace, End, Else));
            p.eat_if(SyntaxKind::RightBrace);
            // eat elses as well
            while p.eat_if(SyntaxKind::Else) {
                p.recover_until(syntax_set!(RightBrace, End));
            }

            p.wrap(open_delim, SyntaxKind::CodeBlock);
            return false;
        }
    }
    code(p, syntax_set!(RightBrace));
    if had_open_brace {
        p.expect_closing_delimiter(open_delim, SyntaxKind::RightBrace);
    } else {
        // No need to error again, we handled the missing braces above
        p.eat_if(SyntaxKind::RightBrace);
    }
    p.wrap(open_delim, SyntaxKind::CodeBlock);

    true
}

fn err_missing_braces(p: &mut Parser, flow: ControlFlow) {
    match flow {
        ControlFlow::If => {
            p.insert_error_before("if expression bodies require braces")
                .with_label_message("Expected an opening `{` after this condition")
                .with_code(&E0005_IF_EXPRESSION_BODIES_REQUIRE_BRACES)
                .with_hint("surround the `if` body with `{}` to define a block");
        }
        ControlFlow::Else => {
            p.insert_error_before("else expression bodies require braces")
                .with_label_message("Expected an opening `{` after `else`")
                .with_code(&E0005_IF_EXPRESSION_BODIES_REQUIRE_BRACES)
                .with_hint("surround the `else` body with `{}` to define a block");
        }
        ControlFlow::While => {
            p.insert_error_before("while expression bodies require braces")
                .with_label_message("Expected an opening `{` after this condition")
                .with_hint("surround the `while` body with `{}` to define a block");
        }
        ControlFlow::For => {
            p.insert_error_before("for expression bodies require braces")
                .with_label_message("Expected an opening `{` after the iterable")
                .with_hint("surround the `for` body with `{}` to define a block");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;

    #[test]
    fn test_parse_while() {
        let mut p = assert_parse(
            r#"
        while true {
            do_thing()
        }
        "#,
        );

        p.assert_next_children(SyntaxKind::WhileLoop, |p| {
            p.assert_next(SyntaxKind::While, "while");
            p.assert_next_children(SyntaxKind::Condition, |p| {
                p.assert_next(SyntaxKind::Bool, "true");
                p.assert_end();
            });
            p.assert_next_children(SyntaxKind::CodeBlock, |p| {
                p.assert_next(SyntaxKind::LeftBrace, "{");
                p.assert_next_children(SyntaxKind::FuncCall, |p| {
                    p.assert_next(SyntaxKind::Ident, "do_thing");
                    p.assert_next_children(SyntaxKind::Args, |p| {
                        p.assert_next(SyntaxKind::LeftParen, "(");
                        p.assert_next(SyntaxKind::RightParen, ")");
                        p.assert_end();
                    });
                });
                p.assert_next(SyntaxKind::RightBrace, "}");
                p.assert_end();
            });
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_while_warns_for_parens() {
        let mut p = assert_parse_with_warnings(
            r#"
        while (true) {
            do_thing()
        }
        "#,
            &[W0002_UNNECESSARY_PARENTHESES_AROUND_CONDITION],
        );

        p.assert_next_children(SyntaxKind::WhileLoop, |p| {
            p.assert_next(SyntaxKind::While, "while");
            p.assert_next_children(SyntaxKind::Condition, |p| {
                p.assert_next_children(SyntaxKind::Parenthesized, |_| {});
                p.assert_next_warning(W0002_UNNECESSARY_PARENTHESES_AROUND_CONDITION);
                p.assert_end();
            });
            p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_parse_if() {
        let mut p = assert_parse(
            r#"
        if true {
            do_thing()
        }
        "#,
        );

        p.assert_next_children(SyntaxKind::Conditional, |p| {
            p.assert_next(SyntaxKind::If, "if");
            p.assert_next_children(SyntaxKind::Condition, |p| {
                p.assert_next(SyntaxKind::Bool, "true");
                p.assert_end();
            });
            p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_if_missing_braces_reports_error() {
        let mut p = assert_parse_with_errors(
            r#"
        if true
            do_thing()
        "#,
            &[E0005_IF_EXPRESSION_BODIES_REQUIRE_BRACES],
        );

        p.assert_next_children(SyntaxKind::Conditional, |p| {
            p.assert_next(SyntaxKind::If, "if");
            p.assert_next_children(SyntaxKind::Condition, |_| {});
            p.assert_next_children(SyntaxKind::CodeBlock, |p| {
                p.assert_next_error(E0005_IF_EXPRESSION_BODIES_REQUIRE_BRACES);
            });
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_if_else_if_else_chain() {
        let mut p = assert_parse(
            r#"
        if cond1 {
            do_one()
        } else if cond2 {
            do_two()
        } else {
            do_fallback()
        }
        "#,
        );

        p.assert_next_children(SyntaxKind::Conditional, |p| {
            p.assert_next(SyntaxKind::If, "if");
            p.assert_next_children(SyntaxKind::Condition, |p| {
                p.assert_next(SyntaxKind::Ident, "cond1");
                p.assert_end();
            });
            p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
            p.assert_next_children(SyntaxKind::ConditionalAlternate, |p| {
                p.assert_next(SyntaxKind::Else, "else");
                p.assert_next(SyntaxKind::If, "if");
                p.assert_next_children(SyntaxKind::Condition, |p| {
                    p.assert_next(SyntaxKind::Ident, "cond2");
                    p.assert_end();
                });
                p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
                p.assert_end();
            });
            p.assert_next_children(SyntaxKind::ConditionalElse, |p| {
                p.assert_next(SyntaxKind::Else, "else");
                p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
                p.assert_end();
            });
            p.assert_end();
        });
        p.assert_end();
    }

    #[test]
    fn test_for_with_wrapped_pattern_warns() {
        let mut p = assert_parse_with_warnings(
            r#"
        for (x in items) {
            do_thing()
        }
        "#,
            &[W0003_UNNECESSARY_PARENTHESES_IN_FOR_EXPRESSION],
        );

        p.assert_next_children(SyntaxKind::ForLoop, |p| {
            p.assert_next(SyntaxKind::For, "for");
            p.assert_next(SyntaxKind::LeftParen, "(");
            p.assert_next(SyntaxKind::Ident, "x");
            p.assert_next(SyntaxKind::In, "in");
            p.assert_next(SyntaxKind::Ident, "items");
            p.assert_next(SyntaxKind::RightParen, ")");
            p.assert_next_warning(W0003_UNNECESSARY_PARENTHESES_IN_FOR_EXPRESSION);
            p.assert_next_children(SyntaxKind::CodeBlock, |_| {});
            p.assert_end();
        });
        p.assert_end();
    }
}
