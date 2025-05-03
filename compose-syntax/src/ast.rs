mod expr;
mod macros;
mod unary;
mod atomics;
mod closure;
mod binary;
mod bindings;

use crate::node::SyntaxNode;
use crate::span::Span;

pub use expr::*;
use macros::*;
pub use unary::*;
pub use atomics::*;
pub use binary::*;
pub use bindings::*;
pub use closure::*;

pub trait AstNode<'a>: Sized {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self>;
    fn to_untyped(self) -> &'a SyntaxNode;
    fn span(self) -> Span {
        self.to_untyped().span()
    }
}


