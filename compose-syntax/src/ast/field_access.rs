use crate::ast::{Expr, Ident};
use crate::ast::macros::node;

node!{
    struct FieldAccess
}

impl<'a> FieldAccess<'a> {
    pub fn target(self) -> Expr<'a> { self.0.cast_first() }
    pub fn field(self) -> Ident<'a> { self.0.cast_last() }
}


#[cfg(test)]
mod tests {
    use crate::assert_ast;
    use super::*;

    #[test]
    fn field_access() {
        assert_ast!(
            "foo.bar",
            field_access as FieldAccess {
                with target: Ident = field_access.target() => {
                    assert_eq!(target.get(), "foo");
                }
                with field: Ident = field_access.field() => {
                    assert_eq!(field.get(), "bar");
                }
            }
        )
    }
}