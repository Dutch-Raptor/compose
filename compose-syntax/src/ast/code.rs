use crate::SyntaxNode;
use crate::ast::macros::node;
use crate::ast::{FnItem, Statement};

node! {
    struct Code
}

impl<'a> Code<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item = FnItem<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }

    pub fn statements(self) -> impl DoubleEndedIterator<Item = Statement<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}
