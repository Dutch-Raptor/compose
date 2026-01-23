use crate::fix::FixBuilder;
use crate::kind::SyntaxKind;
use crate::parser::expressions::{code_expr_prec, code_expression};
use crate::parser::Parser;
use crate::parser::{pattern, ExprContext};
use crate::precedence::Precedence;
use crate::set::{syntax_set, SyntaxSet, ASSIGN_OP};
use crate::{set, PatchEngine, SyntaxNode};
use compose_error_codes::{
    E0003_EXPECTED_BINDING_AFTER_LET, E0006_UNTERMINATED_STATEMENT,
    E0007_MISSING_EQUALS_AFTER_LET_BINDING,
};
use compose_utils::trace_fn;
use ecow::eco_format;
use std::collections::HashSet;

pub(super) fn statement(p: &mut Parser) {
    trace_fn!("parse_statement");

    if p.at(SyntaxKind::LetKW) || (p.at(SyntaxKind::PubKW) && p.peek_at(SyntaxKind::LetKW)) {
        let_binding(p);
        return;
    }

    if p.at(SyntaxKind::BreakKW) {
        break_statement(p);
        return;
    }

    if p.at(SyntaxKind::ContinueKW) {
        continue_statement(p);
        return;
    }

    if p.at(SyntaxKind::ReturnKW) {
        return_statement(p);
        return;
    }

    let m = p.marker();

    code_expr_prec(p, ExprContext::Statement, Precedence::Lowest);

    if p.at_set(ASSIGN_OP) {
        trace_fn!("parse_statement: assignment");
        p.eat();

        code_expression(p);

        p.wrap(m, SyntaxKind::Assignment)
    }
}

pub fn continue_statement(p: &mut Parser) {
    let m = p.marker();

    p.assert(SyntaxKind::ContinueKW);

    p.wrap(m, SyntaxKind::ContinueStatement);
}

pub fn return_statement(p: &mut Parser) {
    let m = p.marker();
    p.assert(SyntaxKind::ReturnKW);

    if p.at_set(set::EXPR) {
        code_expression(p);
    }

    p.wrap(m, SyntaxKind::ReturnStatement)
}

pub fn break_statement(p: &mut Parser) {
    let m = p.marker();
    p.assert(SyntaxKind::BreakKW);

    if p.at_set(set::EXPR) {
        code_expression(p);
    }

    p.wrap(m, SyntaxKind::BreakStatement)
}

pub fn let_binding(p: &mut Parser) {
    trace_fn!("parse_let_binding");
    let m = p.marker();
    p.eat_if(SyntaxKind::PubKW);
    p.assert(SyntaxKind::LetKW);

    let was_mut = p.eat_if(SyntaxKind::MutKW);

    if !p.at_set(set::PATTERN) {
        let got = p.current();

        let err = if got == SyntaxKind::Eq {
            p.insert_error_here("missing binding after `let`")
        } else {
            p.insert_error_here("expected a pattern after `let`")
        };

        err.with_code(&E0003_EXPECTED_BINDING_AFTER_LET)
            .with_label_message(eco_format!(
                "expected a pattern but found `{}`",
                got.descriptive_name()
            ))
            .with_hint(eco_format!(
                "write `let name = ...` or `let mut name = ...`"
            ));

        // eat tokens until we find an `=` or the end of this statement
        p.recover_until(syntax_set!(Eq, End, NewLine, RightBrace));
    } else {
        pattern::pattern(p, false, false, &mut HashSet::new());
    }

    if p.eat_if(SyntaxKind::Eq) {
        code_expression(p);
    } else if p.at_set(set::ATOMIC_EXPR) && !p.had_leading_newline() {
        let pattern_text = p.last_text().to_owned();
        p.insert_error_before("expected `=` after binding name")
            .with_label_message("expected `=` here")
            .with_code(&E0007_MISSING_EQUALS_AFTER_LET_BINDING)
            .with_hint(eco_format!("if you meant to initialize the binding, add `=`: `let {}{} = ...`",
                if was_mut { "mut " } else { "" },
                pattern_text,
            ))
            .with_hint(eco_format!("if you meant to leave it uninitialized, add a semicolon: `let {pattern_text};`"));

        // Assume that the user meant to initialize the binding.
        code_expression(p);
    }

    p.wrap(m, SyntaxKind::LetBinding)
}

pub fn code(p: &mut Parser, end_set: SyntaxSet) {
    let mut pos = p.current_end();
    while !p.end() && !p.at_set(end_set) {
        if p.eat_if(SyntaxKind::Semicolon) {
            continue;
        }

        trace_fn!("code", "loop pos= {}", pos);
        statement(p);

        // Expect the end of an expression. Either a semicolon or a newline.
        if !p.end() && !p.skip_if(SyntaxKind::Semicolon) && !p.at_set(set::STMT_TERMINATOR) {
            let mut engine = PatchEngine::new();
            let stmt_span = p
                .last_node()
                .map(SyntaxNode::span)
                .expect("a node was just added");
            engine
                .insert_after(&stmt_span, ";")
                .expect("only one patch will be added, so no conflicts");

            p
                .insert_error_before("expected a semicolon after a statement")
                .with_code(&E0006_UNTERMINATED_STATEMENT)
                .with_fix(
                    FixBuilder::new("write a semicolon to terminate this statement", stmt_span)
                        .insert_after(&stmt_span, ";")
                        .build(),
                )
                .with_label_message("help: insert a semicolon here");
        }

        // If the parser is not progressing, then we have an error
        if pos == p.current_end() && !p.end() {
            // Eat the token and continue trying to parse
            p.unexpected("Expected a statement", None);
        }
        pos = p.current_end();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;

    #[test]
    fn test_parse_let_binding() {
        assert_parse_tree!("let x = 1;",
            LetBinding [
                LetKW("let")
                Ident("x")
                Eq("=")
                Int("1")
            ]
        );
    }

    #[test]
    fn test_parse_let_mut_binding() {
        assert_parse_tree!("let mut x = 1;",
            LetBinding [
                LetKW("let")
                MutKW("mut")
                Ident("x")
                Eq("=")
                Int("1")
            ]
        );
    }

    #[test]
    fn test_parse_let_binding_uninitialized() {
        assert_parse_tree!("let x;",
            LetBinding [
                LetKW("let")
                Ident("x")
            ]
        );
    }

    #[test]
    fn test_parse_assignment() {
        assert_parse_tree!("x = 1;",
            Assignment [
                Ident("x")
                Eq("=")
                Int("1")
            ]
        );
    }

    #[test]
    fn test_parse_statements() {
        let inputs = &[
            // Semicolons and newlines
            r#"
            let x = 1;
            x += x * 2;
            let y = x
            "#,
            // semicolons
            r#"
            let x = 1; x += x * 2; let y = x;
            "#,
        ];

        for input in inputs {
            assert_parse_tree!(input,
                LetBinding [
                    LetKW("let")
                    Ident("x")
                    Eq("=")
                    Int("1")
                ]
                Assignment [
                    Ident("x")
                    PlusEq("+=")
                    Binary [
                        Ident("x")
                        Star("*")
                        Int("2")
                    ]
                ]
                LetBinding [
                    LetKW("let")
                    Ident("y")
                    Eq("=")
                    Ident("x")
                ]
            );
        }
    }

    #[test]
    fn test_parse_statements_unterminated() {
        assert_parse_tree!("let x = 1 x += x * 2 let y = x",
            LetBinding [
                LetKW("let")
                Ident("x")
                Eq("=")
                Int("1")
            ]
            Error(E0006_UNTERMINATED_STATEMENT)
            Assignment [
                Ident("x")
                PlusEq("+=")
                Binary [
                    Ident("x")
                    Star("*")
                    Int("2")
                ]
            ]
            Error(E0006_UNTERMINATED_STATEMENT)
            LetBinding [
                LetKW("let")
                Ident("y")
                Eq("=")
                Ident("x")
            ]
        );
    }
}
