use crate::ast::{Assignment, AstNode, Expr, LetBinding};
use crate::kind::SyntaxKind;
use crate::SyntaxNode;

pub enum Statement<'a> {
    Expr(Expr<'a>),
    Let(LetBinding<'a>),
    Assign(Assignment<'a>),
}

impl<'a> AstNode<'a> for Statement<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Let => Some(Statement::Let(LetBinding::from_untyped(node)?)),
            SyntaxKind::Assignment => {
                Some(Statement::Assign(Assignment::from_untyped(node)?))
            }
            _ => Expr::from_untyped(node).map(Statement::Expr),
        }
    }

    fn to_untyped(self) -> &'a SyntaxNode {
        match self {
            Statement::Expr(expr) => expr.to_untyped(),
            Statement::Let(let_binding) => let_binding.to_untyped(),
            Statement::Assign(assign_statement) => assign_statement.to_untyped(),
        }
    }
}
