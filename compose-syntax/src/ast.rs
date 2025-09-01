mod expr;
mod macros;
mod unary;
mod atomics;
mod func;
mod binary;
mod bindings;
mod call;
mod field_access;
mod path_access;
mod parenthesized;
mod statement;
mod assignment;
mod control_flow;
mod range;
mod map;
mod module;

use ecow::EcoString;
use crate::node::SyntaxNode;
use crate::span::{HasSpan, Span};

pub use expr::*;
use macros::*;
pub use unary::*;
pub use atomics::*;
pub use binary::*;
pub use bindings::*;
pub use func::*;
pub use call::*;
pub use field_access::*;
pub use path_access::*;
pub use parenthesized::*;
pub use statement::*;
pub use assignment::*;
pub use control_flow::*;
pub use range::*;
pub use map::*;
pub use module::*;

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

impl<'a, T> HasSpan for T where T: AstNode<'a> {
    fn get_span(&self) -> Span {
        AstNode::span(self)
    }
}


