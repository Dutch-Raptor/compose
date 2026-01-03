use crate::ast::func::Pattern;
use crate::ast::{node, AstNode, Expr, Ident};
use crate::kind::SyntaxKind;
use crate::Span;

node! {
    struct LetBinding
}

impl<'a> LetBinding<'a> {
    pub fn has_initial_value(self) -> bool {
        self.0.children().any(|n| n.kind() == SyntaxKind::Eq)
    }

    pub fn initial_value(self) -> Option<Expr<'a>> {
        self.0
            .children()
            .skip_while(|n| n.kind() != SyntaxKind::Eq)
            .nth(1)
            .and_then(|n| n.cast())
    }

    fn bindings(self) -> Vec<Ident<'a>> {
        let pattern: Pattern = self.0.try_cast_first().expect("expected pattern");
        pattern.bindings()
    }

    pub fn pattern(self) -> Pattern<'a> {
        self.0.cast_first()
    }

    pub fn is_mut(self) -> bool {
        self.0.children().any(|n| n.kind() == SyntaxKind::Mut)
    }

    pub fn eq_span(self) -> Span {
        self.0
            .children()
            .find(|&n| n.kind() == SyntaxKind::Eq)
            .map(|n| n.span())
            .unwrap_or_else(|| self.0.span())
    }

    pub fn mut_span(self) -> Option<Span> {
        self.0
            .children()
            .find(|&n| n.kind() == SyntaxKind::Mut)
            .map(|n| n.span())
    }

    pub fn initial_value_span(self) -> Option<Span> {
        self.initial_value().map(|e| e.span())
    }

    pub fn is_public(self) -> bool {
        self.0.children().any(|n| n.kind() == SyntaxKind::Pub)
    }

    pub fn pub_span(self) -> Option<Span> {
        self.0
            .children()
            .find(|&n| n.kind() == SyntaxKind::Pub)
            .map(|n| n.span())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_ast;
    use crate::ast::Int;

    #[test]
    fn test_let_binding() {
        assert_ast!(
            "let x = 1",
            binding as LetBinding {
                binding.bindings().into_iter() => [
                    ident as Ident {
                        assert_eq!(ident.get(), "x");
                    }
                ]
                with init: Int = binding.initial_value().unwrap() => {
                    assert_eq!(init.get(), 1);
                }
            }
        )
    }
}
