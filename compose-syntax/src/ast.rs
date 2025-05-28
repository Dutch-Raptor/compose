mod expr;
mod macros;
mod unary;
mod atomics;
mod closure;
mod binary;
mod bindings;
mod call;
mod field_access;
mod path_access;
mod parenthesized;
mod statement;
mod assignment;

use crate::node::SyntaxNode;
use crate::span::Span;

pub use expr::*;
use macros::*;
pub use unary::*;
pub use atomics::*;
pub use binary::*;
pub use bindings::*;
pub use closure::*;
pub use call::*;
pub use field_access::*;
pub use path_access::*;
pub use parenthesized::*;
pub use statement::*;
pub use assignment::*;

pub trait AstNode<'a>: Sized {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self>;
    fn to_untyped(self) -> &'a SyntaxNode;
    fn span(self) -> Span {
        self.to_untyped().span()
    }
}


