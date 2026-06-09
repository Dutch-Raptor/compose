mod assignment;
mod atomics;
mod binary;
mod bindings;
mod call;
mod code;
mod control_flow;
mod expr;
mod field_access;
mod func;
mod index_access;
mod macros;
mod map;
mod match_expression;
mod module;
mod parenthesized;
mod path_access;
mod pattern;
mod range;
mod statement;
pub mod ty;
mod unary;

use crate::node::SyntaxNode;
use crate::span::{HasSpan, Span};
use ecow::EcoString;

pub use assignment::*;
pub use atomics::*;
pub use binary::*;
pub use bindings::*;
pub use call::*;
pub use code::*;
pub use control_flow::*;
pub use expr::*;
pub use field_access::*;
pub use func::*;
pub use index_access::*;
use macros::*;
pub use map::*;
pub use match_expression::*;
pub use module::*;
pub use parenthesized::*;
pub use path_access::*;
pub use pattern::*;
pub use range::*;
pub use statement::*;
pub use unary::*;

pub trait AstNode<'a>: Sized {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self>;
    fn to_untyped(&self) -> &'a SyntaxNode;
    fn span(&self) -> Span {
        self.to_untyped().span()
    }

    fn to_text(&self) -> EcoString {
        self.to_untyped().to_text()
    }

    fn cast<T: AstNode<'a>>(&self) -> Option<T> {
        self.to_untyped().cast()
    }
}

impl<'a, T> HasSpan for T
where
    T: AstNode<'a>,
{
    fn span(&self) -> Span {
        AstNode::span(self)
    }
}
