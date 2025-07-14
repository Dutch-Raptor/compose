use crate::ast::macros::node;
use crate::ast::{CodeBlock, Expr, Pattern};
use crate::kind::SyntaxKind;
use crate::SyntaxNode;

node! {
    struct Conditional
}

impl<'a> Conditional<'a> {
    /// the condition
    pub fn condition(self) -> Condition<'a> {
        self.0.cast_first()
    }

    /// the consequent for the condition
    pub fn consequent(self) -> CodeBlock<'a> {
        self.0.cast_first()
    }

    /// the else if branches
    pub fn cond_alternates(self) -> impl DoubleEndedIterator<Item = ConditionalAlternate<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }

    /// the else node if any
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
    use crate::assert_ast;
    use crate::ast::{FuncCall, Ident};
    use crate::ast::Str;

    #[test]
    fn test_for_loop() {
        assert_ast!(
            r#"
                for (c in "abc") {
                    println(c);
                }
            "#,
            for_loop as ForLoop {
                with binding: Ident = for_loop.binding() => {
                    assert_eq!(binding.get(), "c");
                }
                with iterable: Str = for_loop.iterable() => {
                    assert_eq!(iterable.get(), "abc");
                }
                with body: CodeBlock = for_loop.body() => {
                    body.statements() => [
                        call as FuncCall {
                            assert_eq!(call.to_text(), "println(c)");
                        }
                    ]
                }
            }
        );
    }

    #[test]
    fn test_while_loop() {
        assert_ast!(
            r#"
                while (true) {
                    println("hello");
                }
            "#,
            while_loop as WhileLoop {
                with condition: Condition = while_loop.condition() => {
                    assert_eq!(condition.expr().to_text(), "true");
                }
                with body: CodeBlock = while_loop.body() => {
                    body.statements() => [
                        call as FuncCall {
                            assert_eq!(call.to_text(), "println(\"hello\")");
                        }
                    ]
                }
            }
        )
    }

    #[test]
    fn test_conditionals() {
        assert_ast!(
            r#"
                if (true) {
                    println("hello");
                } else if (false) {
                    println("bob");
                } else {
                    println("world");
                }
            "#,
            conditional as Conditional {
                with condition: Condition = conditional.condition() => {
                    assert_eq!(condition.expr().to_text(), "true");
                }
                with consequent: CodeBlock = conditional.consequent() => {
                    consequent.statements() => [
                        call as FuncCall {
                            assert_eq!(call.to_text(), "println(\"hello\")");
                        }
                    ]
                }
                conditional.cond_alternates() => [
                    alternate as ConditionalAlternate {
                        with condition: Condition = alternate.condition() => {
                            assert_eq!(condition.expr().to_text(), "false");
                        }
                        with consequent: CodeBlock = alternate.consequent() => {
                            consequent.statements() => [
                                call as FuncCall {
                                    assert_eq!(call.to_text(), "println(\"bob\")");
                                }
                            ]
                        }
                    }
                ]
                with alternate: ConditionalElse = conditional.cond_else().unwrap() => {
                    with consequent: CodeBlock = alternate.consequent() => {
                        consequent.statements() => [
                            call as FuncCall {
                                assert_eq!(call.to_text(), "println(\"world\")");
                            }
                        ]
                    }
                }
            }
        )
    }
}
