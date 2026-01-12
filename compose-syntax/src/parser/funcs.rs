use crate::kind::SyntaxKind;
use crate::parser::expressions::block;
use crate::parser::statements::code;
use crate::parser::{expressions, pattern};
use crate::parser::{ExprContext, Parser};
use crate::precedence::Precedence;
use crate::scanner::Delimiter;
use crate::set;
use crate::set::{syntax_set, ARG_RECOVER};
use compose_error_codes::E0009_ARGS_MISSING_COMMAS;
use compose_utils::trace_fn;
use std::collections::HashSet;

pub fn args(p: &mut Parser) {
    trace_fn!("parse_args");

    debug_assert!(
        p.at_set(syntax_set!(LeftParen, LeftBrace)),
        "Expected `(` or `{{` for args, got {:?}",
        p.current()
    );
    let m = p.marker();

    if p.eat_if(SyntaxKind::LeftParen) {
        while !p.current().is_terminator() {
            arg(p);

            if !p.current().is_terminator() && !p.eat_if(SyntaxKind::Comma) {
                p.insert_error_before("expected a comma between the function arguments")
                    .with_code(&E0009_ARGS_MISSING_COMMAS)
                    .with_label_message("help: insert a comma here");
            }
        }

        p.expect_closing_delimiter(m, SyntaxKind::RightParen);
    }

    if p.at(SyntaxKind::LeftBrace) {
        // trailing lambda
        lambda(p);
    }

    p.wrap(m, SyntaxKind::Args);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockOrLambda {
    Block,
    Lambda,
}

fn lambda(p: &mut Parser) {
    trace_fn!("parse_lambda");
    let m = p.marker();

    p.assert(SyntaxKind::LeftBrace);

    if p.at(SyntaxKind::Pipe) {
        captures(p);
    }

    let mut seen = HashSet::new();

    let param_marker = p.marker();
    while !p.at_set(syntax_set!(Arrow, End)) {
        param(p, &mut seen);

        if !p.at(SyntaxKind::Arrow) && !p.eat_if(SyntaxKind::Comma) {
            p.insert_error_before("expected a comma between the function parameters")
                .with_label_message("help: insert a comma here");

            if p.at_set(set::PARAM) {
                // assume missing comma, continue
                continue;
            }

            // Skip over broken params and recover to the `=>` arrow or closing brace
            if let Ok(Some(arrow)) = p.scanner().find_in_matching_delims(SyntaxKind::Arrow) {
                // Fast path: skip directly to the arrow
                p.recover_until_node(&arrow);
            } else if let Ok(Some(closing)) = p
                .scanner()
                .with_entered_delim(Delimiter::LeftBrace)
                .matching_closing_delim()
            {
                // Fallback: skip to the end of the lambda block
                p.recover_until_node(&closing);
                p.wrap(param_marker, SyntaxKind::Params);
                p.wrap(m, SyntaxKind::Lambda);
                return;
            } else {
                // Give up: consume one token to make progress and accept lower quality diagnostics
                p.eat();
            }
        }
    }

    p.wrap(param_marker, SyntaxKind::Params);

    p.expect(SyntaxKind::Arrow);

    code(p, syntax_set!(RightBrace));

    p.expect_closing_delimiter(m, SyntaxKind::RightBrace);

    p.wrap(m, SyntaxKind::Lambda)
}

/// Parses a block or lambda
///
/// Block syntax: { $(stmt)* }
/// Lambda syntax { $(|capture_list|)? $(param),* $(,)? => $(stmt)* }
pub fn block_or_lambda(p: &mut Parser) -> Option<BlockOrLambda> {
    trace_fn!("parse_block_or_lambda");
    let mut scanner = p.scanner();
    scanner.next(); // skip the opening `{`
    scanner.enter(Delimiter::LeftBrace);

    let contains_arrow = {
        trace_fn!("parse_block_or_lambda_contains_arrow");
        scanner
            .level_contains_kind(SyntaxKind::Arrow)
            .unwrap_or(false)
    };

    if contains_arrow {
        lambda(p);
        Some(BlockOrLambda::Lambda)
    } else {
        block(p);
        Some(BlockOrLambda::Block)
    }
}

fn arg(p: &mut Parser) {
    let m_mods = p.marker();

    let mut had_modifiers = false;
    had_modifiers |= p.eat_if(SyntaxKind::RefKW);
    had_modifiers |= p.eat_if(SyntaxKind::MutKW);

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

    p.eat_if(SyntaxKind::RefKW);
    p.eat_if(SyntaxKind::MutKW);

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
pub fn closure_params(p: &mut Parser) -> bool {
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

    p.eat_if(SyntaxKind::RefKW);
    p.eat_if(SyntaxKind::MutKW);

    let was_at_pat = p.at_set(set::PATTERN);
    let pat_m = p.marker();

    pattern::pattern(p, false, seen, Some("parameter"));

    // Parse named params like `a: 1`
    if p.eat_if(SyntaxKind::Colon) {
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
    use crate::assert_parse_tree;
    use compose_error_codes::E0001_UNCLOSED_DELIMITER;

    #[test]
    fn test_parse_lambda_multiple_params() {
        assert_parse_tree!(
            "{ a, b, c => a + b + c }",
            Lambda [
                LeftBrace("{")
                Params [
                    Param [ Ident("a") ]
                    Comma(",")
                    Param [ Ident("b") ]
                    Comma(",")
                    Param [ Ident("c") ]
                ]
                Arrow("=>")
                Binary [
                    Ident("a")
                    Plus("+")
                    Binary [
                        Ident("b")
                        Plus("+")
                        Ident("c")
                    ]
                ]
                RightBrace("}")
            ]
        );
    }

    #[test]
    fn test_parse_closure_single_param_parens() {
        assert_parse_tree!(
            "{ a => a }",
            Lambda [
                LeftBrace("{")
                Params [
                    Param [ Ident("a") ]
                ]
                Arrow("=>")
                Ident("a")
                RightBrace("}")
            ]
        );
    }

    #[test]
    fn test_parse_closure_discard_param() {
        assert_parse_tree!(
            "{ _ => }",
            Lambda [
                LeftBrace("{")
                Params [
                    Param [ Underscore("_") ]
                ]
                Arrow("=>")
                RightBrace("}")
            ]
        );
    }

    #[test]
    fn test_parse_closure_named_param() {
        assert_parse_tree!(
            "{a: b => }",
            Lambda [
                LeftBrace("{")
                Params [
                    Param [
                        Named [ Ident("a") Colon(":") Ident("b") ]
                    ]
                ]
                Arrow("=>")
                RightBrace("}")
            ]
        );
    }

    #[test]
    fn test_parse_args() {
        assert_parse_tree!(
            "f(a, b, c)",
            FuncCall [
                Ident("f")
                Args [
                    LeftParen("(")
                    Ident("a")
                    Comma(",")
                    Ident("b")
                    Comma(",")
                    Ident("c")
                    RightParen(")")
                ]
            ]
        );
    }

    #[test]
    fn test_parse_args_empty() {
        assert_parse_tree!(
            "f()",
            FuncCall [
                Ident("f")
                Args [
                    LeftParen("(")
                    RightParen(")")
                ]
            ]
        );
    }

    #[test]
    fn test_parse_args_named() {
        assert_parse_tree!(
            "f(a: b)",
            FuncCall [
                Ident("f")
                Args [
                    LeftParen("(")
                    Named [ Ident("a") Colon(":") Ident("b") ]
                    RightParen(")")
                ]
            ]
        );
    }

    #[test]
    fn test_parse_args_named_and_positional() {
        assert_parse_tree!(
            "f(a, b: c)",
            FuncCall [
                Ident("f")
                Args [
                    LeftParen("(")
                    Ident("a")
                    Comma(",")
                    Named [ Ident("b") Colon(":") Ident("c") ]
                    RightParen(")")
                ]
            ]
        );
    }

    #[test]
    fn test_parse_args_malformed() {
        assert_parse_tree!(
            r#"
            f(a, b, c
            1 + 2
            "#,
            FuncCall [
                Ident("f")
                Args [
                    Error(E0001_UNCLOSED_DELIMITER)
                    Ident("a")
                    Comma(",")
                    Ident("b")
                    Comma(",")
                    Ident("c")
                    Error(E0009_ARGS_MISSING_COMMAS)
                    Binary [
                        Int("1")
                        Plus("+")
                        Int("2")
                    ]
                ]
            ]
        );
    }

    #[test]
    fn test_parse_params_ref_mut_combinations() {
        assert_parse_tree!(
            "{ ref a => }",
            Lambda [
                LeftBrace("{")
                Params [
                    Param [
                        RefKW("ref")
                        Ident("a")
                    ]
                ]
                Arrow("=>")
                ...
            ]
        );

        assert_parse_tree!(
            "{ mut a => }",
            Lambda [
                LeftBrace("{")
                Params [
                    Param [
                        MutKW("mut")
                        Ident("a")
                    ]
                ]
                ...
            ]
        );

        assert_parse_tree!(
            "{ ref mut a => }",
            Lambda [
                LeftBrace("{")
                Params [
                    Param [
                        RefKW("ref")
                        MutKW("mut")
                        Ident("a")
                    ]
                ]
                ...
            ]
        );
    }

    #[test]
    fn test_parse_closure_with_capture_list() {
        assert_parse_tree!("{ |a, ref b, mut c, ref mut d| e => }",
            Lambda [
                LeftBrace("{")
                CaptureList [
                    Pipe("|")
                    Capture [ Ident("a") ]
                    Comma(",")
                    Capture [ RefKW("ref") Ident("b") ]
                    Comma(",")
                    Capture [ MutKW("mut") Ident("c") ]
                    Comma(",")
                    Capture [ RefKW("ref") MutKW("mut") Ident("d") ]
                    Pipe("|")
                ]
                Params [
                    Param [ Ident("e") ]
                ]
                Arrow("=>")
                ...
            ]
        );
    }

    #[test]
    fn test_parse_trailing_lambda() {
        assert_parse_tree!("foo() { x => x + 1 }",
            FuncCall [
                Ident("foo")
                Args [
                    LeftParen("(")
                    RightParen(")")
                    Lambda [
                        LeftBrace("{")
                        Params [
                            Param [ Ident("x") ]
                        ]
                        Arrow("=>")
                        Binary [
                            Ident("x")
                            Plus("+")
                            Int("1")
                        ]
                        RightBrace("}")
                    ]
                ]
            ]
        );
    }

    #[test]
    fn test_parse_trailing_lambda_and_args() {
        assert_parse_tree!("foo(1) { x => x + 1 }",
            FuncCall [
                Ident("foo")
                Args [
                    LeftParen("(")
                    Int("1")
                    RightParen(")")
                    Lambda [
                        LeftBrace("{")
                        Params [
                            Param [ Ident("x") ]
                        ]
                        Arrow("=>")
                        Binary [
                            Ident("x")
                            Plus("+")
                            Int("1")
                        ]
                        RightBrace("}")
                    ]
                ]
            ]
        );
    }

    #[test]
    fn test_parse_trailing_lambda_omit_parens() {
        assert_parse_tree!("foo { x => x + 1 }",
            FuncCall [
                Ident("foo")
                Args [
                    Lambda [
                        LeftBrace("{")
                        Params [
                            Param [ Ident("x") ]
                        ]
                        Arrow("=>")
                        Binary [
                            Ident("x")
                            Plus("+")
                            Int("1")
                        ]
                        RightBrace("}")
                    ]
                ]
            ]
        )
    }

    #[test]
    fn test_parse_args_with_named_positional_and_trailing_lambda() {
        assert_parse_tree!("foo(1, 2, a: 1, b: 1) { x => x + 1 }",
            FuncCall [
                Ident("foo")
                Args [
                    LeftParen("(")
                    Int("1")
                    Comma(",")
                    Int("2")
                    Comma(",")
                    Named [ Ident("a") Colon(":") Int("1") ]
                    Comma(",")
                    Named [ Ident("b") Colon(":") Int("1") ]
                    RightParen(")")
                    Lambda [
                        LeftBrace("{")
                        Params [
                            Param [ Ident("x") ]
                        ]
                        Arrow("=>")
                        Binary [
                            Ident("x")
                            Plus("+")
                            Int("1")
                        ]
                        RightBrace("}")
                    ]
                ]
            ]
        )
    }
}
