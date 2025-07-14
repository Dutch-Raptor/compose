use crate::ast::macros::node;
use crate::ast::{Expr, Ident};

node! {
    struct PathAccess
}

impl<'a> PathAccess<'a> {
    pub fn target(self) -> Expr<'a> {
        self.0.cast_first()
    }
    
    pub fn member(self) -> Ident<'a> {
        self.0.cast_last()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_ast;

    #[test]
    fn test_path_access() {
        assert_ast! {
            "foo::bar",
            path as PathAccess {
                with target: Ident = path.target() => {
                    assert_eq!(target.get(), "foo");
                }
                with member: Ident = path.member() => {
                    assert_eq!(member.get(), "bar");
                }
            }
        }
    }
}