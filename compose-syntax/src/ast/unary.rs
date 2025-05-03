use crate::ast::expr::Expr;
use crate::ast::macros::node;
use crate::kind::SyntaxKind;
use crate::precedence::{Precedence, PrecedenceTrait};

node! {
    struct Unary
}

impl<'a> Unary<'a> {
    pub fn op(self) -> UnOp {
        self.0
            .children()
            .find_map(|node| UnOp::from_kind(node.kind()))
            .unwrap_or(UnOp::Plus)
    }

    pub fn expr(self) -> Expr<'a> { self.0.cast_last() }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UnOp {
    Plus,
    Minus,
    Bang,
    Tilde,
}

impl UnOp {
    pub fn from_kind(kind: SyntaxKind) -> Option<Self> {
        Some(match kind {
            SyntaxKind::Plus => Self::Plus,
            SyntaxKind::Minus => Self::Minus,
            SyntaxKind::Bang => Self::Bang,
            SyntaxKind::Tilde => Self::Tilde,
            _ => return None,
        })
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Bang => "!",
            Self::Tilde => "~",
        }
    }
}

impl PrecedenceTrait for UnOp {
    fn precedence(&self) -> Precedence {
        match self {
            Self::Plus | Self::Minus => Precedence::Sum,
            Self::Bang | Self::Tilde => Precedence::Prefix,
        }
    }
}

