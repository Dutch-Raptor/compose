use crate::SyntaxNode;
use crate::ast::macros::node;
use crate::ast::{CodeBlock, Expr, Ident, Pattern};
use crate::kind::SyntaxKind;

node! {
    struct Conditional
}

impl<'a> Conditional<'a> {
    pub fn condition(self) -> Condition<'a> {
        self.0.cast_first()
    }

    pub fn consequent(self) -> CodeBlock<'a> {
        self.0.cast_first()
    }

    pub fn cond_alternates(self) -> impl DoubleEndedIterator<Item = ConditionalAlternate<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }

    pub fn cond_else(self) -> Option<ConditionalElse<'a>> {
        self.0.try_cast_last()
    }
}

node! {
    struct WhileLoop
}

impl<'a> WhileLoop<'a> {
    pub fn condition(self) -> Condition<'a> {
        self.0.cast_first()
    }

    pub fn body(self) -> CodeBlock<'a> {
        self.0.cast_first()
    }
}

node! {
    struct ForLoop
}

#[derive(Debug)]
pub enum ForBinding<'a> {
    Indexed(Pattern<'a>, Ident<'a>),
    Single(Pattern<'a>),
}

impl<'a> ForLoop<'a> {
    pub fn body(self) -> CodeBlock<'a> {
        self.0.cast_last()
    }

    pub fn binding(self) -> Pattern<'a> {
        self.0.cast_first()
    }

    pub fn iterable(self) -> Expr<'a> {
        self.0
            .children()
            .skip_while(|n| n.kind() != SyntaxKind::In)
            .find_map(SyntaxNode::cast)
            .unwrap_or_default()
    }
}

node! {
    struct ConditionalAlternate
}
impl<'a> ConditionalAlternate<'a> {
    pub fn condition(self) -> Condition<'a> {
        self.0.cast_first()
    }

    pub fn consequent(self) -> CodeBlock<'a> {
        self.0.cast_first()
    }
}

node! {
    struct ConditionalElse
}

impl<'a> ConditionalElse<'a> {
    pub fn consequent(self) -> CodeBlock<'a> {
        self.0.cast_first()
    }
}

node! {
    struct Condition
}

impl<'a> Condition<'a> {
    pub fn expr(self) -> Expr<'a> {
        self.0.cast_first()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::AstNode;
    use crate::test_utils::test_parse;

    #[test]
    fn test_for_loop() {
        let nodes = test_parse(
            r#"
            for c in "abc" {
                println(c);
            }
        "#,
        );

        let for_loop = nodes.first().unwrap();

        let typed = for_loop.cast::<ForLoop>().unwrap();

        let pat = typed.binding();

        assert_eq!(pat.to_untyped().text(), "c");

        let iterable = typed.iterable();
        assert_eq!(iterable.to_untyped().text(), "\"abc\"");

        let body = typed.body();
        assert_eq!(
            body.statements().next().unwrap().to_untyped().to_text(),
            "println(c)"
        );
    }
}
