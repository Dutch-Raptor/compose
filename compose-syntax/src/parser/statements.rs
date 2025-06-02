use compose_error_codes::E0003_EXPECTED_BINDING_AFTER_LET;
use compose_utils::trace_fn;
use ecow::eco_format;
use std::collections::HashSet;
use crate::kind::SyntaxKind;
use crate::parser::{patterns, ExprContext};
use crate::parser::Parser;
use crate::precedence::Precedence;
use crate::set;
use crate::parser::expressions::{code_expr_prec, code_expression};
use crate::set::{syntax_set, SyntaxSet, ASSIGN_OP};

pub(super) fn statement(p: &mut Parser) {
    trace_fn!("parse_statement");

    if p.at(SyntaxKind::Let) {
        let_binding(p);
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

pub fn let_binding(p: &mut Parser) {
    trace_fn!("parse_let_binding");
    let m = p.marker();
    p.assert(SyntaxKind::Let);

    p.eat_if(SyntaxKind::Mut);

    if !p.at_set(set::PATTERN) {
        let got = p.current();
        
        let err = if got == SyntaxKind::Eq {
            p.insert_error_here("missing binding after `let`")
        } else {
            p.insert_error_here("expected a pattern after `let`")
        };

        err
            .with_code(&E0003_EXPECTED_BINDING_AFTER_LET)
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
        patterns::pattern(p, false, &mut HashSet::new(), None);
    }

    if p.eat_if(SyntaxKind::Eq) {
        code_expression(p);
    }

    p.wrap(m, SyntaxKind::LetBinding)
}

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