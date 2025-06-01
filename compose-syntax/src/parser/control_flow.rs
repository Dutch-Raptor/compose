use crate::kind::SyntaxKind;
use crate::node::SyntaxErrorSeverity;
use crate::parser::expressions::{code, code_expression};
use crate::parser::patterns::pattern;
use crate::parser::Parser;
use crate::set::syntax_set;
use crate::{set, Span, SyntaxError, SyntaxNode};
use compose_error_codes::{
    E0005_IF_EXPRESSION_BODIES_REQUIRE_BRACES, W0002_UNNECESSARY_PARENTHESES_AROUND_CONDITION,
};
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
            let last = p
                .last_node()
                .map(SyntaxNode::span)
                .unwrap_or(Span::detached());
            let first = p[left_paren_marker].span();
            let span = Span::join(first, last);
            p.insert_error(SyntaxError::new(
                "unnecessary parentheses around loop pattern",
                span,
            ))
            .with_severity(SyntaxErrorSeverity::Warning)
            .with_code(&W0002_UNNECESSARY_PARENTHESES_AROUND_CONDITION)
            .with_label_message("help: remove these parentheses");
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
