use crate::ast::Expr;
use crate::ast::macros::node;
use crate::kind::SyntaxKind;

node! {
    struct Assignment
}

impl<'a> Assignment<'a> {
    pub fn lhs(self) -> Expr<'a> {
        self.0.cast_first()
    }
    
    pub fn op(self) -> AssignOp {
        self.0.children().find_map(|n| AssignOp::from_kind(n.kind()))
            .unwrap_or(AssignOp::Assign)
    }
    
    pub fn rhs(self) -> Expr<'a> {
        self.0.cast_last()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

impl AssignOp {
    pub(crate) fn from_kind(kind: SyntaxKind) -> Option<Self> {
        Some(match kind {
            SyntaxKind::Eq => AssignOp::Assign,
            SyntaxKind::PlusEq => AssignOp::AddAssign,
            SyntaxKind::MinusEq => AssignOp::SubAssign,
            SyntaxKind::StarEq => AssignOp::MulAssign,
            SyntaxKind::SlashEq => AssignOp::DivAssign,
            SyntaxKind::AmpersandEq => AssignOp::ModAssign,
            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::assert_ast;
    use crate::ast::{Ident, Int};
    use super::*;

    #[test]
    fn test_assign_op() {
        assert_eq!(AssignOp::from_kind(SyntaxKind::Eq), Some(AssignOp::Assign));
        assert_eq!(AssignOp::from_kind(SyntaxKind::PlusEq), Some(AssignOp::AddAssign));
        assert_eq!(AssignOp::from_kind(SyntaxKind::MinusEq), Some(AssignOp::SubAssign));
        assert_eq!(AssignOp::from_kind(SyntaxKind::StarEq), Some(AssignOp::MulAssign));
        assert_eq!(AssignOp::from_kind(SyntaxKind::SlashEq), Some(AssignOp::DivAssign));
        assert_eq!(AssignOp::from_kind(SyntaxKind::AmpersandEq), Some(AssignOp::ModAssign));
    }

    #[test]
    fn assignment() {
        assert_ast!(
            "a = 23",
            assignment as Assignment {
                with lhs: Ident = assignment.lhs() => {
                    assert_eq!(lhs.get(), "a");
                }
                with rhs: Int = assignment.rhs() => {
                    assert_eq!(rhs.get(), 23);
                }
            }
        )
    }
}