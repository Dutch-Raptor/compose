use crate::ast::Expr;
use crate::ast::macros::node;

node! {
    struct Parenthesized
}

impl<'a> Parenthesized<'a> {
    pub fn expr(self) -> Expr<'a> {
        self.0.cast_first()
    }
}