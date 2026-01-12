use crate::ast::{Assignment, AstNode, Expr, LetBinding};
use crate::ast::macros::node;
use crate::ast::module::ModuleImport;
use crate::kind::SyntaxKind;
use crate::SyntaxNode;

#[derive(Debug, Clone, Copy)]
pub enum Statement<'a> {
    Expr(Expr<'a>),
    Let(LetBinding<'a>),
    Assign(Assignment<'a>),
    Break(BreakStatement<'a>),
    Return(ReturnStatement<'a>),
    Continue(ContinueStatement<'a>),
    ModuleImport(ModuleImport<'a>)
}

impl<'a> AstNode<'a> for Statement<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::LetBinding => Some(Statement::Let(LetBinding::from_untyped(node)?)),
            SyntaxKind::Assignment => {
                Some(Statement::Assign(Assignment::from_untyped(node)?))
            }
            SyntaxKind::BreakStatement => Some(Statement::Break(BreakStatement::from_untyped(node)?)),
            SyntaxKind::ReturnStatement => Some(Statement::Return(ReturnStatement::from_untyped(node)?)),
            SyntaxKind::ContinueKW => Some(Statement::Continue(ContinueStatement::from_untyped(node)?)),
            SyntaxKind::ModuleImport => Some(Statement::ModuleImport(ModuleImport::from_untyped(node)?)),
            _ => Expr::from_untyped(node).map(Statement::Expr),
        }
    }

    fn to_untyped(&self) -> &'a SyntaxNode {
        match self {
            Statement::Expr(expr) => expr.to_untyped(),
            Statement::Let(let_binding) => let_binding.to_untyped(),
            Statement::Assign(assign_statement) => assign_statement.to_untyped(),
            Statement::Break(break_statement) => break_statement.to_untyped(),
            Statement::Return(return_statement) => return_statement.to_untyped(),
            Statement::Continue(continue_statement) => continue_statement.to_untyped(),
            Statement::ModuleImport(import) => import.to_untyped(),
        }
    }
}

node! {
    struct BreakStatement
}

impl<'a> BreakStatement<'a> {
    pub fn value(self) -> Option<Expr<'a>> {
        self.0.children().find_map(SyntaxNode::cast)
    }
}

node! {
    struct ReturnStatement
}

impl<'a> ReturnStatement<'a> {
    pub fn value(self) -> Option<Expr<'a>> {
        self.0.children().find_map(SyntaxNode::cast)
    }
}

node! {
    struct ContinueStatement
}
