use crate::ast::{node, Expr};
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
    use crate::assert_ast;
    use crate::ast::{Int};

    #[test]
    fn test_binop() {
        let binop = BinOp::from_kind(SyntaxKind::Plus).unwrap();
        assert_eq!(binop, BinOp::Add);
    }

    #[test]
    fn test_precedence_equal() {
        assert_ast!("1 + 2 + 3",
            bin as Binary {
                assert_eq!(bin.op(), BinOp::Add);
                with lhs: Int = bin.lhs() => {
                    assert_eq!(lhs.get(), 1);
                }
                with rhs: Binary = bin.rhs() => {
                    assert_eq!(rhs.op(), BinOp::Add);
                    with inner_lhs: Int = rhs.lhs() => {
                        assert_eq!(inner_lhs.get(), 2);
                    }
                    with inner_rhs: Int = rhs.rhs() => {
                        assert_eq!(inner_rhs.get(), 3);
                    }
                }
            }
        );
    }

    #[test]
    fn test_precedence_higher() {
        assert_ast!("1 + 2 * 3",
            bin as Binary {
                assert_eq!(bin.op(), BinOp::Add);
                with lhs: Int = bin.lhs() => {
                    assert_eq!(lhs.get(), 1);
                }
                with rhs: Binary = bin.rhs() => {
                    assert_eq!(rhs.op(), BinOp::Mul);
                    with inner_lhs: Int = rhs.lhs() => {
                        assert_eq!(inner_lhs.get(), 2);
                    }
                    with inner_rhs: Int = rhs.rhs() => {
                        assert_eq!(inner_rhs.get(), 3);
                    }
                }
            }
        );
        assert_ast!(
            "1 * 2 + 3",
            bin as Binary {
                assert_eq!(bin.op(), BinOp::Add);
                with lhs: Binary = bin.lhs() => {
                    assert_eq!(lhs.op(), BinOp::Mul);
                    with inner_lhs: Int = lhs.lhs() => {
                        assert_eq!(inner_lhs.get(), 1);
                    }
                    with inner_rhs: Int = lhs.rhs() => {
                        assert_eq!(inner_rhs.get(), 2);
                    }
                }
                with rhs: Int = bin.rhs() => {
                    assert_eq!(rhs.get(), 3);
                }
            }
        );
    }
}
