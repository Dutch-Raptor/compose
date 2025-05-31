use crate::kind::SyntaxKind;
use crate::node::SyntaxErrorSeverity;
use crate::parser::expressions::{code, code_expression};
use crate::parser::Parser;
use crate::set::syntax_set;
use crate::{set, SyntaxError};
use compose_error_codes::{E0005_IF_EXPRESSION_BODIES_REQUIRE_BRACES, W0002_UNNECESSARY_PARENTHESES_AROUND_CONDITION};

pub(crate) fn conditional(p: &mut Parser) {
    let m = p.marker();
    p.assert(SyntaxKind::If);

    // Parse the condition
    condition(p);

    if !parse_conditional_block(p) {
        p.wrap(m, SyntaxKind::Conditional);
        return;
    }

    while p.at(SyntaxKind::Else) {
        let else_marker = p.marker();
        p.assert(SyntaxKind::Else);
        if p.at(SyntaxKind::If) {
            p.assert(SyntaxKind::If);
            // another condition
            condition(p);
            
            if !parse_conditional_block(p) {
                p.wrap(m, SyntaxKind::Conditional);
                return;
            }
            p.wrap(else_marker, SyntaxKind::ConditionalAlternate);
            continue;
        } 
        if !parse_conditional_block(p) {
            p.wrap(m, SyntaxKind::Conditional);
            return;
        }
        
        p.wrap(else_marker, SyntaxKind::ConditionalElse);
    }
    
    p.wrap(m, SyntaxKind::Conditional);
}

fn condition(p: &mut Parser) {
    let cond_marker = p.marker();
    code_expression(p);
    let last_node = p.last_node().unwrap();
    if last_node.kind() == SyntaxKind::Parenthesized {
        p.insert_error(SyntaxError::new("unnecessary parentheses around condition", last_node.span()))
            .with_severity(SyntaxErrorSeverity::Warning)
            .with_code(&W0002_UNNECESSARY_PARENTHESES_AROUND_CONDITION)
            .with_label_message("help: remove these parentheses");
    }
    p.wrap(cond_marker, SyntaxKind::Condition);
}

fn parse_conditional_block(p: &mut Parser) -> bool {
    let open_delim = p.marker();
    let had_open_brace = p.at(SyntaxKind::LeftBrace);
    if !p.eat_if(SyntaxKind::LeftBrace) {
        p.insert_error_before("if expression bodies require braces")
            .with_label_message("Expected an opening `{` after this condition")
            .with_code(&E0005_IF_EXPRESSION_BODIES_REQUIRE_BRACES)
            .with_hint("surround the if body with `{}` to define a block");

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

            return false;
        }
    }
    code(p, syntax_set!(RightBrace));
    if had_open_brace {
        p.expect_closing_delimiter(open_delim, SyntaxKind::RightBrace);
    } else {
        p.eat_if(SyntaxKind::RightBrace);       
    }
    p.wrap(open_delim, SyntaxKind::CodeBlock);
    
    true
}
