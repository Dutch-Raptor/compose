use crate::parser::Parser;
use crate::SyntaxKind;

pub fn parse_type_annotation(p: &mut Parser) {
    let m = p.marker();
    if p.at(SyntaxKind::LeftParen) {
        // Tuple
        unimplemented!("parse tuple type");
    }

    p.expect(SyntaxKind::Ident);
    
    p.wrap(m, SyntaxKind::TypeAnnotation);
}
