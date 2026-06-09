use crate::ast::pattern::Pattern;
use crate::ast::ty::{Type, TypeArgs};
use crate::ast::{AstNode, CodeBlock, Expr, Ident, Statement, node};
use crate::kind::SyntaxKind;
use crate::{Span, SyntaxNode};

node! {
    struct Lambda
}

impl<'a> Lambda<'a> {
    pub fn params(self) -> Params<'a> {
        self.0.cast_first()
    }

    pub fn captures(self) -> CaptureList<'a> {
        self.0.cast_first()
    }

    pub fn statements(self) -> impl Iterator<Item = Statement<'a>> {
        self.0
            .children()
            .skip_while(|n| n.kind() != SyntaxKind::Arrow)
            .filter_map(SyntaxNode::cast)
    }

    pub fn items(self) -> impl Iterator<Item = FnItem<'a>> {
        self.0
            .children()
            .skip_while(|n| n.kind() != SyntaxKind::Arrow)
            .filter_map(SyntaxNode::cast)
    }
}

node! {
    struct FnItem
}

impl<'a> FnItem<'a> {
    pub fn name(self) -> Ident<'a> {
        self.0.cast_first()
    }

    pub fn type_args(self) -> TypeArgs<'a> {
        self.0.cast_first()
    }

    pub fn params(self) -> Params<'a> {
        self.0.cast_first()
    }

    pub fn return_type(self) -> Option<Type<'a>> {
        self.0.children().rev().find_map(SyntaxNode::cast)
    }

    pub fn body(self) -> CodeBlock<'a> {
        self.0.cast_first()
    }
}

node! {
    struct CaptureList
}

impl<'a> CaptureList<'a> {
    pub fn children(self) -> impl DoubleEndedIterator<Item = Capture<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

node! {
    struct Capture
}

impl<'a> Capture<'a> {
    pub fn binding(self) -> Ident<'a> {
        self.0.cast_first()
    }

    pub fn is_ref(self) -> bool {
        self.0.children().any(|n| n.kind() == SyntaxKind::RefKW)
    }

    pub fn ref_span(self) -> Option<Span> {
        self.0
            .children()
            .find(|n| n.kind() == SyntaxKind::RefKW)
            .map(|n| n.span())
    }

    pub fn is_mut(self) -> bool {
        self.0.children().any(|n| n.kind() == SyntaxKind::MutKW)
    }

    pub fn mut_span(self) -> Option<Span> {
        self.0
            .children()
            .find(|n| n.kind() == SyntaxKind::MutKW)
            .map(|n| n.span())
    }
}

node! {
    struct Params
}

impl<'a> Params<'a> {
    pub fn children(self) -> impl DoubleEndedIterator<Item = Param<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

node! {
    struct Param
}

impl<'a> Param<'a> {
    pub fn kind(self) -> ParamKind<'a> {
        self.0.cast_first()
    }

    pub fn type_annotation(self) -> Option<Type<'a>> {
        fn find_type(node: &SyntaxNode) -> Option<Type<'_>> {
            node.cast().or_else(|| node.children().find_map(find_type))
        }

        self.0.children().find_map(find_type)
    }

    pub fn is_ref(self) -> bool {
        self.0.children().any(|n| n.kind() == SyntaxKind::RefKW)
    }

    pub fn ref_span(self) -> Option<Span> {
        self.0
            .children()
            .find(|n| n.kind() == SyntaxKind::RefKW)
            .map(|n| n.span())
    }

    pub fn is_mut(self) -> bool {
        self.0.children().any(|n| n.kind() == SyntaxKind::MutKW)
    }

    pub fn mut_span(self) -> Option<Span> {
        self.0
            .children()
            .find(|n| n.kind() == SyntaxKind::MutKW)
            .map(|n| n.span())
    }
}

#[derive(Debug)]
pub enum ParamKind<'a> {
    // A positional parameter `x`
    Pos(Pattern<'a>),
    // A named parameter `help = "try it like this"`
    Named(Named<'a>),
}

impl<'a> Default for ParamKind<'a> {
    fn default() -> Self {
        Self::Pos(Pattern::default())
    }
}

impl<'a> AstNode<'a> for ParamKind<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Named => Some(Self::Named(Named::from_untyped(node)?)),
            _ => node.cast().map(Self::Pos),
        }
    }

    fn to_untyped(&self) -> &'a SyntaxNode {
        match self {
            Self::Named(n) => n.to_untyped(),
            Self::Pos(p) => p.to_untyped(),
        }
    }
}

node! {
    struct Underscore
}

node! {
    struct Named
}

impl<'a> Named<'a> {
    pub fn name(self) -> Ident<'a> {
        self.0.cast_first()
    }

    pub fn expr(self) -> Expr<'a> {
        self.0.cast_last()
    }

    /// The right hand of the pair as a pattern.
    ///
    /// This should only be used in `destructuring`
    pub fn pattern(self) -> Pattern<'a> {
        self.0.cast_last()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_ast;
    use crate::ast::FuncCall;
    use crate::ast::binary::{BinOp, Binary};
    use crate::ast::ty::Type;

    #[test]
    fn trailing_lambda() {
        assert_ast!(
            r#"
            foo() { a => a + b; }
            "#,
            call as FuncCall {
                with callee: Ident = call.callee() => {
                    assert_eq!(callee.get(), "foo");
                }
                call.args().items() => [
                    trailing_lambda as Lambda {
                        with params: Params = trailing_lambda.params() => {
                            params.children() => [
                                param as Param {
                                    with pat: Pattern = param.kind() => {
                                        with ident: Ident = pat => {
                                            assert_eq!(ident.get(), "a");
                                        }
                                    }
                                }
                            ]
                        }

                        trailing_lambda.statements() => [
                            binary as Binary {
                                with lhs: Ident = binary.lhs() => {
                                    assert_eq!(lhs.get(), "a");
                                }
                                with rhs: Ident = binary.rhs() => {
                                    assert_eq!(rhs.get(), "b");
                                }
                                assert_eq!(binary.op(), BinOp::Add);
                            }
                        ]
                    }
                ]
            }
        )
    }

    #[test]
    fn fn_item() {
        assert_ast!(
            r#"
            fn add<T>(a: T, b: T) -> T { a + b }
            "#,
            item as FnItem {
                with name: Ident = item.name() => {
                    assert_eq!(name.get(), "add");
                }
                item.type_args().items() => [
                    ty as Type {
                        with ident: Ident = ty.ident() => {
                            assert_eq!(ident.get(), "T");
                        }
                    }
                ]
                with params: Params = item.params() => {
                    params.children() => [
                        param as Param {
                            with pat: Pattern = param.kind() => {
                                with ident: Ident = pat => {
                                    assert_eq!(ident.get(), "a");
                                }
                            }
                        }
                        param as Param {
                            with pat: Pattern = param.kind() => {
                                with ident: Ident = pat => {
                                    assert_eq!(ident.get(), "b");
                                }
                            }
                        }
                    ]
                }
                with return_type: Type = item.return_type().unwrap() => {
                    with ident: Ident = return_type.ident() => {
                        assert_eq!(ident.get(), "T");
                    }
                }
                with body: CodeBlock = item.body() => {
                    body.statements() => [
                        binary as Binary {
                            with lhs: Ident = binary.lhs() => {
                                assert_eq!(lhs.get(), "a");
                            }
                            with rhs: Ident = binary.rhs() => {
                                assert_eq!(rhs.get(), "b");
                            }
                            assert_eq!(binary.op(), BinOp::Add);
                        }
                    ]
                }
            }
        )
    }
}
