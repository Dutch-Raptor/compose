use crate::parser::Parser;
use crate::SyntaxKind;

pub fn parse_type_annotation(p: &mut Parser) {
    let m = p.marker();
    if p.at(SyntaxKind::LeftParen) {
        // Tuple
        unimplemented!("parse tuple type");
    }

    p.expect(SyntaxKind::Ident);

    if p.eat_if(SyntaxKind::Lt) {
        while !p.at_set(crate::set::syntax_set!(Gt, End)) {
            parse_type_annotation(p);

            if p.at(SyntaxKind::Gt) {
                break;
            }

            if !p.eat_if(SyntaxKind::Comma) {
                p.insert_error_before("expected a comma between type arguments")
                    .with_label_message("help: insert a comma here");

                if !p.at(SyntaxKind::Ident) {
                    break;
                }
            }
        }

        p.expect(SyntaxKind::Gt);
    }

    p.wrap(m, SyntaxKind::TypeAnnotation);
}
