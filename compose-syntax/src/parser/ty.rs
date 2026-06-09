use crate::SyntaxKind;
use crate::parser::Parser;
use compose_error_codes::E0009_ARGS_MISSING_COMMAS;
use compose_utils::trace_fn;

pub fn parse_type(p: &mut Parser) {
    let m = p.marker();
    if p.at(SyntaxKind::LeftParen) {
        // Tuple
        unimplemented!("parse tuple type");
    }

    p.expect(SyntaxKind::Ident);

    if p.at(SyntaxKind::Lt) {
        parse_type_args(p);
    }

    p.wrap(m, SyntaxKind::Type);
}

pub fn parse_type_args(p: &mut Parser) {
    trace_fn!("parse_type_args");
    let m = p.marker();

    p.expect(SyntaxKind::Lt);

    while !p.current().is_terminator() {
        parse_type(p);

        if !p.current().is_terminator() && !p.eat_if(SyntaxKind::Comma) {
            p.insert_error_before("expected a comma between the type arguments")
                .with_code(&E0009_ARGS_MISSING_COMMAS)
                .with_label_message("help: insert a comma here");
        }
    }
    p.expect_closing_delimiter(m, SyntaxKind::Gt);

    p.wrap(m, SyntaxKind::TypeArgs);
}
