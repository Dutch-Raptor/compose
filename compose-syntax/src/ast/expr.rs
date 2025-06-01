use crate::SyntaxNode;
use crate::ast::atomics::Unit;
use crate::ast::unary::Unary;
use crate::ast::{AstNode, Binary, ForLoop, Ident, Int, Parenthesized, WhileLoop};
use crate::ast::{Bool, Closure, CodeBlock, FieldAccess, FuncCall, LetBinding, PathAccess, Str};
use crate::ast::control_flow::Conditional;
use crate::kind::SyntaxKind;

/// An expression. The base of Compose. Any "statement" is an expression.
///
/// # Examples:
///
/// ```compose
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
    Str(Str<'a>),
    Bool(Bool<'a>),
    FuncCall(FuncCall<'a>),
    FieldAccess(FieldAccess<'a>),
    PathAccess(PathAccess<'a>),
    Closure(Closure<'a>),
    Parenthesized(Parenthesized<'a>),
    Conditional(Conditional<'a>),
    WhileLoop(WhileLoop<'a>),
    ForLoop(ForLoop<'a>),
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
            SyntaxKind::Str => Some(Self::Str(Str::from_untyped(node)?)),
            SyntaxKind::Bool => Some(Self::Bool(Bool::from_untyped(node)?)),
            SyntaxKind::FuncCall => Some(Self::FuncCall(FuncCall::from_untyped(node)?)),
            SyntaxKind::FieldAccess => Some(Self::FieldAccess(FieldAccess::from_untyped(node)?)),
            SyntaxKind::PathAccess => Some(Self::PathAccess(PathAccess::from_untyped(node)?)),
            SyntaxKind::Closure => Some(Self::Closure(Closure::from_untyped(node)?)),
            SyntaxKind::Parenthesized => Some(Self::Parenthesized(Parenthesized::from_untyped(node)?)),
            SyntaxKind::Conditional => Some(Self::Conditional(Conditional::from_untyped(node)?)),
            SyntaxKind::WhileLoop => Some(Self::WhileLoop(WhileLoop::from_untyped(node)?)),       
            SyntaxKind::ForLoop => Some(Self::ForLoop(ForLoop::from_untyped(node)?)),       
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
            Self::Str(str) => str.to_untyped(),
            Self::Bool(bool) => bool.to_untyped(),
            Self::FuncCall(func_call) => func_call.to_untyped(),
            Self::FieldAccess(field_access) => field_access.to_untyped(),
            Self::PathAccess(path_access) => path_access.to_untyped(),
            Self::Closure(closure) => closure.to_untyped(),
            Self::Parenthesized(parenthesized) => parenthesized.to_untyped(),       
            Self::Conditional(conditional) => conditional.to_untyped(),       
            Self::WhileLoop(while_loop) => while_loop.to_untyped(),       
            Self::ForLoop(for_loop) => for_loop.to_untyped(),       
        }
    }
}

impl Default for Expr<'_> {
    fn default() -> Self {
        Self::Unit(Unit::default())
    }
}
