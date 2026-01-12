use crate::kind::SyntaxKind;
use crate::parser::Parser;
use crate::parser::expressions::code_expression;
use crate::parser::{ExprContext, expressions};
use crate::precedence::Precedence;
use crate::set::syntax_set;
use crate::{SyntaxError, set};
use compose_utils::trace_fn;
use ecow::eco_format;
use std::collections::HashSet;
use std::mem;
use itertools::Itertools;

pub fn match_expr(p: &mut Parser) {
    trace_fn!("parse_match");
    let m = p.marker();
    p.assert(SyntaxKind::MatchKW);

    let wrapped = p.eat_if(SyntaxKind::LeftParen);
    if !wrapped {
        p.insert_error_here("condition expressions require parentheses")
            .with_label_message("Expected an opening `(` before the condition");

        p.recover_until(syntax_set!(LeftBrace));

        // error is not recoverable within the parens, try to recover until after and abort
        return;
    }

    if wrapped {
        code_expression(p);
        p.expect_closing_delimiter(m, SyntaxKind::RightParen);
    }

    let open_marker = p.marker();
    p.expect(SyntaxKind::LeftBrace);

    while !p.current().is_terminator() {
        // match arm
        let arm_m = p.marker();

        // patterns
        let mut first_bindings = HashSet::new();
        pattern(p, false, &mut first_bindings, None);
        while p.eat_if(SyntaxKind::Pipe) {
            let mut current_bindings = HashSet::new();
            pattern(p, false, &mut current_bindings, None);

            if current_bindings != first_bindings {
                insert_pattern_bindings_mismatch_error(p, &mut first_bindings, &mut current_bindings);
            }
        }

        // guard
        if p.eat_if(SyntaxKind::IfKW) {
            code_expression(p);
        }

        if !p.eat_if(SyntaxKind::Arrow) {
            p.insert_error_here("expected `=>` in match arm")
                .with_label_message("expected `=>` here");

            if !p.at_set(set::EXPR) {
                // If the `=>` is not missing, just eat a token to make progress
                p.eat();
            }
            continue;
        }

        code_expression(p);

        p.wrap(arm_m, SyntaxKind::MatchArm);

        if !p.current().is_terminator() {
            p.expect_or_recover(SyntaxKind::Comma, syntax_set!(RightBracket));
        }
    }

    p.expect_closing_delimiter(open_marker, SyntaxKind::RightBrace);

    p.wrap(m, SyntaxKind::MatchExpression)
}

fn insert_pattern_bindings_mismatch_error(p: &mut Parser, first_bindings: &mut HashSet<&str>, current_bindings: &mut HashSet<&str>) {
    let span = p.last_node().unwrap().span();

    let mut introduced = current_bindings
        .difference(&first_bindings)
        .peekable();
    let mut missing = first_bindings
        .difference(&current_bindings)
        .peekable();

    let mut diff_note = eco_format!("pattern bindings mismatch");

    if missing.peek().is_some() {
        diff_note.push_str(&eco_format!(
                        "\n  - not bound here: `{}`",
                        missing.join("`, `")
                    ))
    }
    if introduced.peek().is_some() {
        diff_note.push_str(&eco_format!(
                        "\n  - bound only here: `{}`",
                        introduced.join("`, `")
                    ));
    };

    p.insert_error_at(span, "patterns within a match arm have differing bindings")
        .with_hint("all patterns joined with `|` must bind the same variables, because the arm body must work for every pattern")
        .with_hint(diff_note);
}

/// Parses a binding or reassignment pattern.
pub fn pattern<'s>(
    p: &mut Parser<'s>,
    reassignment: bool,
    seen: &mut HashSet<&'s str>,
    dupe: Option<&'s str>,
) {
    trace_fn!("parse_pattern");
    match p.current() {
        // Literals
        SyntaxKind::Int | SyntaxKind::Float | SyntaxKind::Str | SyntaxKind::Bool => p.eat(),
        SyntaxKind::LeftParen if p.peek_at(SyntaxKind::RightParen) => {
            let m = p.marker();
            p.assert(SyntaxKind::LeftParen);
            p.assert(SyntaxKind::RightParen);
            p.wrap(m, SyntaxKind::Unit);
        }

        SyntaxKind::Underscore => p.eat(),
        SyntaxKind::LeftBracket => destructure_array(p, seen),
        _ => pattern_leaf(p, reassignment, seen, dupe),
    }
}

fn destructure_array<'s>(p: &mut Parser<'s>, seen: &mut HashSet<&'s str>) {
    trace_fn!("parse_destructure_array");

    let m = p.marker();
    p.assert(SyntaxKind::LeftBracket);

    let mut sink = false;
    while !p.current().is_terminator() {
        if !p.at_set(set::DESTRUCTURING_ITEM) {
            p.unexpected("expected a destructuring item", None);
            continue;
        }

        destructuring_item(p, seen, &mut sink);

        if !p.current().is_terminator() {
            p.expect_or_recover(SyntaxKind::Comma, syntax_set!(RightBracket));
        }
    }

    p.expect_closing_delimiter(m, SyntaxKind::RightBracket);

    p.wrap(m, SyntaxKind::Destructuring);
}

fn destructuring_item<'s>(p: &mut Parser<'s>, seen: &mut HashSet<&'s str>, sink: &mut bool) {
    let m = p.marker();

    if p.eat_if(SyntaxKind::Dots) {
        // sink
        if p.at_set(set::PATTERN_LEAF) {
            pattern_leaf(p, false, seen, None);
        }
        p.wrap(m, SyntaxKind::Spread);
        if mem::replace(sink, true) {
            p[m].convert_to_error("duplicate spread operator (`..`) in destructuring pattern");
        }
        return;
    }

    // parse normal array element or map key
    pattern(p, false, seen, None);

    if p.eat_if(SyntaxKind::Colon) {
        pattern(p, true, seen, None);

        if p[m].kind() != SyntaxKind::Ident {
            p[m].expected("identifier after `:` in destructuring pattern")
        }

        p.wrap(m, SyntaxKind::Named)
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

    if p.eat_if(SyntaxKind::Underscore) {
        return;
    }

    let m = p.marker();
    let text = p.current_text();

    // Parse a full atomic expression, even though we only care about an identifier.
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

fn parenthesized_or_destructuring(
    p: &mut Parser,
    _reassignment: bool,
    _seen: &mut HashSet<&str>,
    _dupe: Option<&str>,
) {
    trace_fn!("parse_destructuring");
    p.assert(SyntaxKind::At);

    unimplemented!("destructuring")
}

#[cfg(test)]
mod tests {
    use crate::assert_parse_tree;

    #[test]
    fn test_destructuring_underscore() {
        assert_parse_tree!("let _ = 2",
            LetBinding [
                LetKW("let")
                Underscore("_")
                Eq("=")
                Int("2")
            ]
        );
    }

    #[test]
    fn test_destructuring_array() {
        assert_parse_tree!("let [a, b, ..c] = 2",
            LetBinding [
                LetKW("let")
                    Destructuring [
                        LeftBracket("[")
                            Ident("a")
                            Comma(",")
                            Ident("b")
                            Comma(",")
                            Spread [
                                Dots("..")
                                Ident("c")
                            ]
                        RightBracket("]")
                    ]
                Eq("=")
                Int("2")
            ]
        )
    }
}
