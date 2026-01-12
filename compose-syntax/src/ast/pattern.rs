use crate::ast::{AstNode, Expr, Ident, Named, Underscore, Unit};
use crate::{SyntaxKind, SyntaxNode};
use crate::ast::macros::node;

#[derive(Debug, Clone, Copy)]
pub enum Pattern<'a> {
    Single(Expr<'a>),
    PlaceHolder(Underscore<'a>),
    Destructuring(Destructuring<'a>),
    LiteralPattern(LiteralPattern<'a>)
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
            SyntaxKind::Underscore => Some(Self::PlaceHolder(Underscore::from_untyped(node)?)),
            SyntaxKind::Destructuring => {
                Some(Self::Destructuring(Destructuring::from_untyped(node)?))
            }
            _ => {
                match LiteralPattern::from_untyped(node) {
                    Some(lit) => Some(Self::LiteralPattern(lit)),
                    None => node.cast().map(Self::Single),
                }
            }
        }
    }

    fn to_untyped(&self) -> &'a SyntaxNode {
        match self {
            Self::Single(e) => e.to_untyped(),
            Self::PlaceHolder(u) => u.to_untyped(),
            Self::Destructuring(d) => d.to_untyped(),
            Self::LiteralPattern(lit) => lit.to_untyped(),
        }
    }
}

impl Default for Pattern<'_> {
    fn default() -> Self {
        Self::Single(Expr::default())
    }
}


#[derive(Debug, Clone, Copy)]
pub enum LiteralPattern<'a> {
    Int(Expr<'a>),
    Float(Expr<'a>),
    Str(Expr<'a>),
    Bool(Expr<'a>),
    Unit(Unit<'a>),
}

impl<'a> AstNode<'a> for LiteralPattern<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Int => Some(Self::Int(Expr::from_untyped(node)?)),
            SyntaxKind::Float => Some(Self::Float(Expr::from_untyped(node)?)),
            SyntaxKind::Str => Some(Self::Str(Expr::from_untyped(node)?)),
            SyntaxKind::Bool => Some(Self::Bool(Expr::from_untyped(node)?)),
            SyntaxKind::Unit => Some(Self::Unit(Unit::from_untyped(node)?)),
            _ => None,
        }
    }

    fn to_untyped(&self) -> &'a SyntaxNode {
        match self {
            Self::Int(i) => i.to_untyped(),
            Self::Float(f) => f.to_untyped(),
            Self::Str(s) => s.to_untyped(),
            Self::Bool(b) => b.to_untyped(),
            Self::Unit(u) => u.to_untyped(),
        }
    }
}

node! {
    struct Destructuring
}

node! {
    struct Spread
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
                DestructuringItem::Spread(spread) => spread.sink_ident().into_iter().collect(),
            })
            .collect()
    }
}

pub enum DestructuringItem<'a> {
    Pattern(Pattern<'a>),
    Named(Named<'a>),
    Spread(Spread<'a>),
}

impl<'a> Spread<'a> {
    /// The spread expression.
    ///
    /// This should only be accessed if this `Spread` is contained in an
    /// `ArrayItem`, `MapItem`, or `Arg`.
    pub fn expr(self) -> Expr<'a> {
        self.0.cast_first()
    }

    /// The sink identifier, if present.
    ///
    /// This should only be accessed if this `Spread` is contained in a
    /// `Param` or binding `DestructuringItem`.
    pub fn sink_ident(self) -> Option<Ident<'a>> {
        self.0.try_cast_first()
    }

    /// The sink expressions, if present.
    ///
    /// This should only be accessed if this `Spread` is contained in a
    /// `DestructuringItem`.
    pub fn sink_expr(self) -> Option<Expr<'a>> {
        self.0.try_cast_first()
    }
}

impl<'a> AstNode<'a> for DestructuringItem<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Named => Some(Self::Named(Named::from_untyped(node)?)),
            SyntaxKind::Spread => Some(Self::Spread(Spread(node))),
            _ => node.cast().map(Self::Pattern),
        }
    }

    fn to_untyped(&self) -> &'a SyntaxNode {
        match self {
            Self::Named(n) => n.to_untyped(),
            Self::Pattern(p) => p.to_untyped(),
            Self::Spread(s) => s.to_untyped(),
        }
    }
}
