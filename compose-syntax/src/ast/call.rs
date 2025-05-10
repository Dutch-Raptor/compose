use crate::SyntaxNode;
use crate::ast::macros::node;
use crate::ast::{AstNode, Expr};

node! {
    struct FuncCall
}

impl<'a> FuncCall<'a> {
    pub fn callee(self) -> Expr<'a> {
        self.0.cast_first()
    }
    pub fn args(self) -> Args<'a> {
        self.0.cast_last()
    }
}

node! {
    struct Args
}

impl<'a> Args<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item = Arg<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Arg<'a> {
    Pos(Expr<'a>),
}

impl<'a> AstNode<'a> for Arg<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        node.cast().map(Self::Pos)
    }

    fn to_untyped(self) -> &'a SyntaxNode {
        match self {
            Arg::Pos(p) => p.to_untyped(),
        }
    }
}
