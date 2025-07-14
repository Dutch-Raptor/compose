use std::collections::HashSet;
use compose_utils::trace_fn;
use ecow::eco_format;
use crate::kind::SyntaxKind;
use crate::parser::{expressions, ExprContext};
use crate::parser::Parser;
use crate::precedence::Precedence;
use crate::{set, SyntaxError};

/// Parses a binding or reassignment pattern.
pub fn pattern<'s>(
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
        p.insert_error_here("expected a pattern");
        return;
    }

    let m = p.marker();
    let text = p.current_text();

    // Parse a full expression, even though we only care about an identifier.
    // This way the entire expression can be marked as an error if it is not.
    expressions::code_expr_prec(p, ExprContext::AtomicExpr, Precedence::Lowest);

    // If the pattern is not a reassignment, it can only be an identifier
    if !reassignment {
        let node = &mut p[m];
        if node.kind() != SyntaxKind::Ident {
            let span = node.span();
            p.insert_error(SyntaxError::new("expected a pattern", span));
            return;
        }
        if !seen.insert(text) {
            node.convert_to_error(eco_format!(
                "duplicate {binding}: {text}",
                binding = dupe.unwrap_or("binding")
            ));
        }
    }
}

fn destructuring(p: &mut Parser, _reassignment: bool, _seen: &mut HashSet<&str>, _dupe: Option<&str>) {
    trace_fn!("parse_destructuring");
    p.assert(SyntaxKind::At);

    unimplemented!("destructuring")
}