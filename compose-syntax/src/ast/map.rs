use crate::SyntaxNode;
use crate::ast::Expr;
use crate::ast::macros::node;

node! {
    struct MapLiteral
}

impl<'a> MapLiteral<'a> {
    pub fn entries(self) -> impl DoubleEndedIterator<Item = MapEntry<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

node! {
    struct MapEntry
}

impl<'a> MapEntry<'a> {
    pub fn key(self) -> Expr<'a> {
        self.0.cast_first()
    }

    pub fn value(self) -> Expr<'a> {
        self.0.cast_last()
    }
}
