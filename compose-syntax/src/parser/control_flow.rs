use crate::kind::SyntaxKind;
use crate::parser::Parser;
use crate::parser::expressions::code_expression;
use crate::parser::pattern::pattern;
use crate::parser::statements::code;
use crate::set::syntax_set;
use crate::{SyntaxErrorSeverity, set, Label};
use compose_error_codes::E0005_IF_EXPRESSION_BODIES_REQUIRE_BRACES;
use compose_utils::trace_fn;
use std::collections::HashSet;
use ecow::eco_format;

pub(crate) fn conditional(p: &mut Parser) {
    trace_fn!("parse_conditional");
    let m = p.marker();
    p.assert(SyntaxKind::IfKW);

    condition(p, "if");

    if !parse_control_flow_block(p, ControlFlow::If) {
        p.wrap(m, SyntaxKind::Conditional);
        return;
    }

    while p.at(SyntaxKind::ElseKW) {
        trace_fn!("parse_else_maybe_if");
        let else_marker = p.marker();
        p.assert(SyntaxKind::ElseKW);
        if p.eat_if(SyntaxKind::IfKW) {
            trace_fn!("parse_else_if");
            condition(p, "else if");

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
    p.assert(SyntaxKind::WhileKW);

    condition(p, "while");

    parse_control_flow_block(p, ControlFlow::While);
    p.wrap(m, SyntaxKind::WhileLoop);
}

pub fn for_loop(p: &mut Parser) {
    trace_fn!("parse_for_loop");
    let m = p.marker();
    p.assert(SyntaxKind::ForKW);

    let left_paren_marker = p.marker();

    let wrapped = p.eat_if(SyntaxKind::LeftParen);

    if !wrapped {
        p.insert_error_here("for loop patterns require parentheses")
            .with_label_message("Expected an opening `(` after the `for` keyword");
    }

    pattern(p, true, &mut HashSet::new(), None);

    p.expect(SyntaxKind::InKW);

    // parse the iterable
    code_expression(p);

    if wrapped {
        p.expect_closing_delimiter(left_paren_marker, SyntaxKind::RightParen);
    }

    parse_control_flow_block(p, ControlFlow::For);

    p.wrap(m, SyntaxKind::ForLoop);
}

fn condition(p: &mut Parser, control_flow: &str) {
    trace_fn!("parse_condition");
    let m = p.marker();

    let wrapped = p.eat_if(SyntaxKind::LeftParen);

    code_expression(p);

    if wrapped {
        let span = p.current_span();
        p.expect_closing_delimiter(m, SyntaxKind::RightParen);

        p.insert_error_at(p[m].span(), eco_format!("parentheses are not required around {control_flow} conditions"))
            .with_severity(SyntaxErrorSeverity::Warning)
            .with_label(Label::primary(span, "remove these parentheses"));

        p.recover_until(syntax_set!(LeftBrace));
    }

    p.wrap(m, SyntaxKind::Condition);
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
            p.recover_until(syntax_set!(RightBrace, End, ElseKW));
            p.eat_if(SyntaxKind::RightBrace);
            // eat elses as well
            while p.eat_if(SyntaxKind::ElseKW) {
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
        assert_parse_tree!(
            r#"
            while (true) {
                do_thing();
            }
            "#,
            WhileLoop [
                WhileKW("while")
                Condition [
                    LeftParen("(")
                    Bool("true")
                    RightParen(")")
                ]
                CodeBlock [
                    LeftBrace("{")
                    FuncCall [
                        Ident("do_thing")
                        Args [ LeftParen("(") RightParen(")") ]
                    ]
                    RightBrace("}")
                ]
            ]
        );
    }

    #[test]
    fn test_parse_if() {
        assert_parse_tree!(
            r#"
            if (true) {
                do_thing();
            }
            "#,
            Conditional [
                IfKW("if")
                Condition [
                    LeftParen("(")
                    Bool("true")
                    RightParen(")")
                ]
                CodeBlock [...]
            ]
        );
    }

    #[test]
    fn test_if_missing_braces_reports_error() {
        assert_parse_tree!(r#"
            if (true)
                do_thing();
            "#,
            Conditional [
                IfKW("if")
                Condition [ ... ]
                CodeBlock [
                    Error(E0005_IF_EXPRESSION_BODIES_REQUIRE_BRACES)
                    FuncCall [
                        Ident("do_thing")
                        Args [ LeftParen("(") RightParen(")") ]
                    ]
                ]
            ]
        );
    }

    #[test]
    fn test_if_else_if_else_chain() {
        assert_parse_tree!(r#"
            if (cond1) {
                do_one();
            } else if (cond2) {
                do_two();
            } else {
                do_fallback();
            }
            "#,
            Conditional [
                IfKW("if")
                Condition [
                    LeftParen("(")
                    Ident("cond1")
                    RightParen(")")
                ]
                CodeBlock [...]
                ConditionalAlternate [
                    ElseKW("else")
                    IfKW("if")
                    Condition [
                        LeftParen("(")
                        Ident("cond2")
                        RightParen(")")
                    ]
                    CodeBlock [...]
                ]
                ConditionalElse [
                    ElseKW("else")
                    CodeBlock [...]
                ]
            ]
        );
    }
}
