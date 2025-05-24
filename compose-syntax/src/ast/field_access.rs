use crate::ast::{Expr, Ident};
use crate::ast::macros::node;

node!{
    struct FieldAccess
}

impl<'a> FieldAccess<'a> {
    pub fn target(self) -> Expr<'a> { self.0.cast_first() }
    pub fn field(self) -> Ident<'a> { self.0.cast_last() }
}