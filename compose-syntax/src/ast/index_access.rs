use crate::ast::Expr;
use crate::ast::macros::node;


node! {
    /// A `target[index]` index access.
    struct IndexAccess
}

impl<'a> IndexAccess<'a> {

    /// The target of the index expression
    pub fn target(self) -> Expr<'a> {
        self.0.cast_first()
    }

    /// The index of the index expression
    pub fn index(self) -> Expr<'a> {
        self.0.cast_last()
    }
}