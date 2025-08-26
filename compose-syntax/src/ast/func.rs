use crate::ast::{node, AstNode, Expr, Ident, Statement};
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

    pub fn statements(self) -> impl Iterator<Item=Statement<'a>> {
        self.0
            .children()
            .skip_while(|n| n.kind() != SyntaxKind::Arrow)
            .filter_map(SyntaxNode::cast)
    }
}

node! {
    struct CaptureList
}

impl<'a> CaptureList<'a> {
    pub fn children(self) -> impl DoubleEndedIterator<Item=Capture<'a>> {
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
        self.0.children().any(|n| n.kind() == SyntaxKind::Ref)
    }

    pub fn ref_span(self) -> Option<Span> {
        self.0
            .children()
            .find(|n| n.kind() == SyntaxKind::Ref)
            .map(|n| n.span())
    }

    pub fn is_mut(self) -> bool {
        self.0.children().any(|n| n.kind() == SyntaxKind::Mut)
    }

    pub fn mut_span(self) -> Option<Span> {
        self.0
            .children()
            .find(|n| n.kind() == SyntaxKind::Mut)
            .map(|n| n.span())
    }
}

node! {
    struct Params
}

impl<'a> Params<'a> {
    pub fn children(self) -> impl DoubleEndedIterator<Item=Param<'a>> {
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

    pub fn is_ref(self) -> bool {
        self.0.children().any(|n| n.kind() == SyntaxKind::Ref)
    }

    pub fn ref_span(self) -> Option<Span> {
        self.0
            .children()
            .find(|n| n.kind() == SyntaxKind::Ref)
            .map(|n| n.span())
    }

    pub fn is_mut(self) -> bool {
        self.0.children().any(|n| n.kind() == SyntaxKind::Mut)
    }

    pub fn mut_span(self) -> Option<Span> {
        self.0
            .children()
            .find(|n| n.kind() == SyntaxKind::Mut)
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

#[derive(Debug, Clone, Copy)]
pub enum Pattern<'a> {
    Single(Expr<'a>),
    PlaceHolder(Underscore<'a>),
    Destructuring(Destructuring<'a>),
}

impl<'a> Pattern<'a> {
    pub fn bindings(self) -> Vec<Ident<'a>> {
        match self {
            Pattern::Single(Expr::Ident(i)) => vec![i],
            Pattern::Destructuring(v) => v.bindings(),
            _ => vec![],
        }
    }
}

impl<'a> AstNode<'a> for Pattern<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Underscore => Some(Self::PlaceHolder(Underscore(node))),
            SyntaxKind::Destructuring => Some(Self::Destructuring(Destructuring(node))),
            _ => node.cast().map(Self::Single),
        }
    }

    fn to_untyped(&self) -> &'a SyntaxNode {
        match self {
            Self::Single(e) => e.to_untyped(),
            Self::PlaceHolder(u) => u.to_untyped(),
            Self::Destructuring(d) => d.to_untyped(),
        }
    }
}

impl Default for Pattern<'_> {
    fn default() -> Self {
        Self::Single(Expr::default())
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

    pub fn pattern(self) -> Pattern<'a> {
        self.0.cast_last()
    }
}

node! {
    struct Destructuring
}

impl<'a> Destructuring<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item=DestructuringItem<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }

    pub fn bindings(self) -> Vec<Ident<'a>> {
        self.items()
            .flat_map(|binding| match binding {
                DestructuringItem::Named(named) => named.pattern().bindings(),
                DestructuringItem::Pattern(pattern) => pattern.bindings(),
            })
            .collect()
    }
}

pub enum DestructuringItem<'a> {
    Pattern(Pattern<'a>),
    Named(Named<'a>),
}

impl<'a> AstNode<'a> for DestructuringItem<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Named => Some(Self::Named(Named::from_untyped(node)?)),
            _ => node.cast().map(Self::Pattern),
        }
    }

    fn to_untyped(&self) -> &'a SyntaxNode {
        match self {
            Self::Named(n) => n.to_untyped(),
            Self::Pattern(p) => p.to_untyped(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_ast;
    use crate::ast::binary::{BinOp, Binary};
    use crate::ast::FuncCall;

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
}
