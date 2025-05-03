use crate::ast::{node, Expr, Ident};
use crate::ast::closure::Pattern;
use crate::kind::SyntaxKind;
use crate::SyntaxNode;

node! {
    struct LetBinding
}

impl<'a> LetBinding<'a> {
    pub fn initial_value(self) -> Option<Expr<'a>> {
        self.0.children().filter_map(SyntaxNode::cast).nth(1)
    }
    
    fn bindings(self) -> Vec<Ident<'a>> {
        let pattern: Pattern = self.0.try_cast_first().expect("expected pattern");
        pattern.bindings()
    }
    
    pub fn pattern(self) -> Pattern<'a> {
        self.0.cast_first()
    }
    
    pub fn is_mut(self) -> bool {
        self.0.children().find(|&n| n.kind() == SyntaxKind::Mut).is_some()
    }
}


#[cfg(test)]
mod tests {
    use crate::ast::AstNode;
    use crate::test_utils::test_parse;
    use super::*;
    
    #[test]
    fn test_let_binding() {
        let node = test_parse("let x = 1").pop().unwrap();
        let binding: LetBinding = node.cast().unwrap();
        
        let init = binding.initial_value().unwrap();
        assert_eq!(init.to_untyped().text(), "1");
        
        let bindings = binding.bindings();
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].to_untyped().text(), "x");
    }
}