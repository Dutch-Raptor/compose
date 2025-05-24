use crate::ast::{Expr, Ident};
use crate::ast::macros::node;

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
    use crate::ast::AstNode;
    use crate::kind::SyntaxKind;
    use crate::test_utils::test_parse;
    use super::*;
    
    #[test]
    fn test_path_access() {
        let mut nodes = test_parse("foo::bar");
        assert_eq!(nodes.len(), 1);
        let node = nodes.pop().unwrap();
        assert_eq!(node.kind(), SyntaxKind::PathAccess);
        
        let path_access: PathAccess = node.cast().unwrap();
        
        let target = path_access.target();
        assert_eq!(target.to_untyped().text(), "foo");
        let member = path_access.member();
        assert_eq!(member.to_untyped().text(), "bar");
    }
}