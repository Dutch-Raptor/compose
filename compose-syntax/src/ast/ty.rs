use crate::ast::macros::node;
use crate::ast::Ident;
use crate::SyntaxNode;

node! {
    struct TypeAnnotation
}

impl<'a> TypeAnnotation<'a> {
    pub fn ident(self) -> Ident<'a> {
        self.0.cast_first()
    }

    pub fn args(self) -> impl DoubleEndedIterator<Item = TypeAnnotation<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}
