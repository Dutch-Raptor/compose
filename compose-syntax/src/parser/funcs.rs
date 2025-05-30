use crate::kind::SyntaxKind;
use crate::parser::Parser;
use crate::parser::{expressions, patterns};
use crate::set::{syntax_set, ARG_RECOVER};
use crate::set;
use compose_utils::trace_fn;
use std::collections::HashSet;

pub fn args(p: &mut Parser) {
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
    expressions::code_expression(p);

    if p.eat_if(SyntaxKind::Colon) {
        if p[m].kind() != SyntaxKind::Ident {
            p[m].expected("identifier");
        }
        expressions::code_expression(p);
        p.wrap(m, SyntaxKind::Named)
    }
}

pub fn closure(p: &mut Parser) {
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

    expressions::code_expression(p);

    p.wrap(m, SyntaxKind::Closure)
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
    patterns::pattern(p, false, seen, Some("parameter"));

    // Parse named params like `a = 1`
    if p.eat_if(SyntaxKind::Eq) {
        if was_at_pat && p[m].kind() != SyntaxKind::Ident {
            p[m].expected("identifier");
        }

        expressions::code_expression(p);
        p.wrap(m, SyntaxKind::Named)
    }
}