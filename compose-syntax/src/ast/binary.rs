use crate::ast::{Expr, node};
use crate::kind::SyntaxKind;
use crate::precedence::{Precedence, PrecedenceTrait};

pub enum Assoc {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,

    BitAnd,
    BitOr,
    BitXor,
    BitShl,
    BitShr,
}

impl BinOp {
    pub(crate) fn from_kind(kind: SyntaxKind) -> Option<Self> {
        Some(match kind {
            SyntaxKind::Plus => Self::Add,
            SyntaxKind::Minus => Self::Sub,
            SyntaxKind::Star => Self::Mul,
            SyntaxKind::Slash => Self::Div,
            SyntaxKind::Percent => Self::Mod,
            SyntaxKind::AmpAmp => Self::And,
            SyntaxKind::PipePipe => Self::Or,

            SyntaxKind::EqEq => Self::Eq,
            SyntaxKind::BangEq => Self::Neq,
            SyntaxKind::Lt => Self::Lt,
            SyntaxKind::LtEq => Self::Lte,
            SyntaxKind::Gt => Self::Gt,
            SyntaxKind::GtEq => Self::Gte,

            SyntaxKind::Amp => Self::BitAnd,
            SyntaxKind::Pipe => Self::BitOr,
            SyntaxKind::Hat => Self::BitXor,
            SyntaxKind::LtLt => Self::BitShl,
            SyntaxKind::GtGt => Self::BitShr,

            _ => return None,
        })
    }

    pub fn descriptive_name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::And => "&&",
            Self::Or => "||",
            Self::Eq => "==",
            Self::Neq => "!=",
            Self::Lt => "<",
            Self::Lte => "<=",
            Self::Gt => ">",
            Self::Gte => ">=",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::BitShl => "<<",
            Self::BitShr => ">>",
        }
    }

    pub fn assoc(self) -> Assoc {
        match self {
            Self::Add | Self::Sub | Self::Mul | Self::Div | Self::Mod => Assoc::Left,
            Self::BitAnd | Self::BitOr | Self::BitXor | Self::BitShl | Self::BitShr => Assoc::Left,
            Self::And | Self::Or => Assoc::Left,
            Self::Eq | Self::Neq | Self::Lt | Self::Lte | Self::Gt | Self::Gte => Assoc::Left,
        }
    }
}

impl PrecedenceTrait for BinOp {
    fn precedence(&self) -> Precedence {
        match self {
            BinOp::Add | BinOp::Sub => Precedence::Sum,
            BinOp::Mul | BinOp::Div | BinOp::Mod => Precedence::Product,
            BinOp::And => Precedence::LogicalAnd,
            BinOp::Or => Precedence::LogicalOr,
            BinOp::Eq | BinOp::Neq => Precedence::Equals,
            BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte => Precedence::LessGreater,
            BinOp::BitAnd => Precedence::BitwiseAnd,
            BinOp::BitOr => Precedence::BitwiseOr,
            BinOp::BitXor => Precedence::BitwiseXor,
            BinOp::BitShl | BinOp::BitShr => Precedence::BitShift,
        }
    }
}

node! {
    struct Binary
}

impl<'a> Binary<'a> {
    pub fn lhs(self) -> Expr<'a> {
        self.0.cast_first()
    }

    pub fn rhs(self) -> Expr<'a> {
        self.0.cast_last()
    }

    pub fn op(self) -> BinOp {
        self.0
            .children()
            .find_map(|n| BinOp::from_kind(n.kind()))
            .unwrap_or(BinOp::Add)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AstNode, Int};
    use crate::test_utils::test_parse;

    #[test]
    fn test_binop() {
        let binop = BinOp::from_kind(SyntaxKind::Plus).unwrap();
        assert_eq!(binop, BinOp::Add);
    }

    #[test]
    fn test_precedence_equal() {
        let node = test_parse("1 + 2 + 3").pop().unwrap();

        let outer_bin: Binary = node.cast().unwrap();

        assert_eq!(outer_bin.op(), BinOp::Add);

        let lhs_int: Int = outer_bin.lhs().to_untyped().cast().unwrap();
        assert_eq!(lhs_int.get(), 1);

        let rhs_bin: Binary = outer_bin.rhs().to_untyped().cast().unwrap();
        assert_eq!(rhs_bin.op(), BinOp::Add);

        let rhs_bin_lhs: Int = rhs_bin.lhs().to_untyped().cast().unwrap();
        assert_eq!(rhs_bin_lhs.get(), 2);
        let rhs_bin_rhs: Int = rhs_bin.rhs().to_untyped().cast().unwrap();
        assert_eq!(rhs_bin_rhs.get(), 3);
    }

    #[test]
    fn test_precedence_higher() {
        {
            let node = test_parse("1 + 2 * 3").pop().unwrap();

            let outer_bin: Binary = node.cast().unwrap();

            assert_eq!(outer_bin.op(), BinOp::Add);

            let lhs_int: Int = outer_bin.lhs().to_untyped().cast().unwrap();
            assert_eq!(lhs_int.get(), 1);

            let rhs_bin: Binary = outer_bin.rhs().to_untyped().cast().unwrap();
            assert_eq!(rhs_bin.op(), BinOp::Mul);

            let rhs_bin_lhs: Int = rhs_bin.lhs().to_untyped().cast().unwrap();
            assert_eq!(rhs_bin_lhs.get(), 2);
            let rhs_bin_rhs: Int = rhs_bin.rhs().to_untyped().cast().unwrap();
            assert_eq!(rhs_bin_rhs.get(), 3);
        }

        {
            let node = test_parse("2 * 3 + 1").pop().unwrap();

            let outer_bin: Binary = node.cast().unwrap();

            assert_eq!(outer_bin.op(), BinOp::Add);

            let lhs_bin: Binary = outer_bin.lhs().to_untyped().cast().unwrap();
            assert_eq!(lhs_bin.op(), BinOp::Mul);

            let lhs_bin_lhs: Int = lhs_bin.lhs().to_untyped().cast().unwrap();
            assert_eq!(lhs_bin_lhs.get(), 2);
            let lhs_bin_rhs: Int = lhs_bin.rhs().to_untyped().cast().unwrap();
            assert_eq!(lhs_bin_rhs.get(), 3);

            let rhs_int: Int = outer_bin.rhs().to_untyped().cast().unwrap();
            assert_eq!(rhs_int.get(), 1);
        }
    }
}
