use crate::SyntaxNode;
use crate::ast::Statement;
use crate::ast::macros::node;

node! {
    struct Code
}

impl<'a> Code<'a> {
    pub fn statements(self) -> impl DoubleEndedIterator<Item = Statement<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}
