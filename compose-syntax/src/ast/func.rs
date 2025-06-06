use crate::ast::{AstNode, Expr, Ident, node};
use crate::kind::SyntaxKind;
use crate::{Span, SyntaxNode};

node! {
    struct Closure
}

impl<'a> Closure<'a> {
    pub fn params(self) -> Params<'a> {
        self.0.cast_first()
    }

    pub fn body(self) -> Expr<'a> {
        self.0.cast_last()
    }
}

node! {
    struct Params
}

impl<'a> Params<'a> {
    pub fn children(self) -> impl DoubleEndedIterator<Item = ParamKind<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

#[derive(Debug)]
pub enum Param<'a> {
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

    fn to_untyped(self) -> &'a SyntaxNode {
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
    pub(crate) fn bindings(self) -> Vec<Ident<'a>> {
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

    fn to_untyped(self) -> &'a SyntaxNode {
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
    pub fn items(self) -> impl DoubleEndedIterator<Item = DestructuringItem<'a>> {
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

    fn to_untyped(self) -> &'a SyntaxNode {
        match self {
            Self::Named(n) => n.to_untyped(),
            Self::Pattern(p) => p.to_untyped(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::binary::{BinOp, Binary};
    use crate::test_utils::test_parse;

    #[test]
    fn parse_single_param_closure() {
        let nodes = test_parse("a => a");
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].kind(), SyntaxKind::Closure);
        let closure: Closure = nodes[0].cast().unwrap();

        assert_eq!(closure.params().children().count(), 1);
        let first_param = closure.params().children().next().unwrap();
        let pat = match first_param {
            ParamKind::Pos(p) => p,
            ParamKind::Named(n) => panic!("Expected positional param, got named param {n:?}"),
        };

        let expr = match pat {
            Pattern::Single(e) => e,
            other => panic!("Expected single pattern, got {other:?}"),
        };

        let ident = match expr {
            Expr::Ident(i) => i,
            other => panic!("Expected ident, got {other:?}"),
        };

        assert_eq!(ident.get(), "a");
    }

    #[test]
    fn parse_multiple_param_closure() {
        let nodes = test_parse("(a, b) => a - b");
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].kind(), SyntaxKind::Closure);
        let closure: Closure = nodes[0].cast().unwrap();

        let params = closure.params();
        assert_eq!(params.children().count(), 2);

        let first = params.children().next().unwrap();
        match first {
            ParamKind::Pos(p) => {
                let pat = match p {
                    Pattern::Single(e) => e,
                    other => panic!("Expected single pattern, got {other:?}"),
                };

                let expr = match pat {
                    Expr::Ident(i) => i,
                    other => panic!("Expected ident, got {other:?}"),
                };

                assert_eq!(expr.get(), "a");
            }
            other => {
                panic!("Expected positional param, got {other:?}");
            }
        }

        let second = params.children().nth(1).unwrap();
        match second {
            ParamKind::Pos(p) => {
                let pat = match p {
                    Pattern::Single(e) => e,
                    other => panic!("Expected single pattern, got {other:?}"),
                };

                let expr = match pat {
                    Expr::Ident(i) => i,
                    other => panic!("Expected ident, got {other:?}"),
                };

                assert_eq!(expr.get(), "b");
            }
            other => {
                panic!("Expected positional param, got {other:?}");
            }
        }

        let body = closure.body();

        let as_bin_op: Binary = body.to_untyped().cast().unwrap();
        assert_eq!(as_bin_op.op(), BinOp::Sub);

        assert_eq!(as_bin_op.lhs().to_untyped().kind(), SyntaxKind::Ident);
        assert_eq!(as_bin_op.lhs().to_untyped().text(), "a");

        assert_eq!(as_bin_op.rhs().to_untyped().kind(), SyntaxKind::Ident);
        assert_eq!(as_bin_op.rhs().to_untyped().text(), "b");
    }
}
