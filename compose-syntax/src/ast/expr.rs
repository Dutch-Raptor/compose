use crate::ast::{CodeBlock, LetBinding};
use crate::ast::{AstNode, Binary, Ident, Int};
use crate::ast::atomics::Unit;
use crate::ast::unary::Unary;
use crate::kind::SyntaxKind;
use crate::SyntaxNode;

/// An expression. The base of Compose. Any "statement" is an expression.
///
/// # Examples:
///
/// ```comp
/// 1 + 2
/// foobar()
/// foo.bar()[0]
/// let a = (2 / 5) + (7 - 2) * foobar()
/// ```
#[derive(Debug, Clone, Copy)]
pub enum Expr<'a> {
    Unary(Unary<'a>),
    Unit(Unit<'a>),
    Ident(Ident<'a>),
    Binary(Binary<'a>),
    Int(Int<'a>),
    LetBinding(LetBinding<'a>),   
    CodeBlock(CodeBlock<'a>),
}

impl<'a> AstNode<'a> for Expr<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Unary => Some(Self::Unary(Unary::from_untyped(node)?)),
            SyntaxKind::Unit => Some(Self::Unit(Unit::from_untyped(node)?)),
            SyntaxKind::Ident => Some(Self::Ident(Ident::from_untyped(node)?)),
            SyntaxKind::Binary => Some(Self::Binary(Binary::from_untyped(node)?)),       
            SyntaxKind::Int => Some(Self::Int(Int::from_untyped(node)?)),       
            SyntaxKind::LetBinding => Some(Self::LetBinding(LetBinding::from_untyped(node)?)),
            SyntaxKind::CodeBlock => Some(Self::CodeBlock(CodeBlock::from_untyped(node)?)),       
            _ => None,
        }
    }

    fn to_untyped(self) -> &'a SyntaxNode {
        match self {
            Self::Unary(unary) => unary.to_untyped(),
            Self::Unit(unit) => unit.to_untyped(),
            Self::Ident(ident) => ident.to_untyped(),
            Self::Binary(binary) => binary.to_untyped(),       
            Self::Int(int) => int.to_untyped(),       
            Self::LetBinding(let_binding) => let_binding.to_untyped(),
            Self::CodeBlock(code_block) => code_block.to_untyped(),       
        }
    }
}

impl Default for Expr<'_> {
    fn default() -> Self {
        Self::Unit(Unit::default())
    }
}
