use crate::ast::macros::node;
use crate::ast::Expr;
use crate::ast::Pattern;
use crate::{SyntaxKind, SyntaxNode};

node! {
    struct MatchExpression
}

impl<'a> MatchExpression<'a> {
    pub fn expr(self) -> Expr<'a> {
        self.0.cast_first()
    }

    pub fn match_arms(self) -> impl DoubleEndedIterator<Item = MatchArm<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

node! {
    struct MatchArm
}

impl<'a> MatchArm<'a> {
    pub fn patterns(self) -> impl Iterator<Item = Pattern<'a>> {
        self.0
            .children()
            .take_while(|node| !matches!(node.kind(), SyntaxKind::Arrow | SyntaxKind::IfKW))
            .filter_map(SyntaxNode::cast)
    }

    pub fn guard(self) -> Option<Expr<'a>> {
        self.0
            .children()
            // Dont look past the arrow
            .take_while(|node| !matches!(node.kind(), SyntaxKind::Arrow))
            // Get the expression after the `if` keyword
            .skip_while(|node| !matches!(node.kind(), SyntaxKind::IfKW))
            .skip(1) // skip the `if` keyword
            .find_map(SyntaxNode::cast)
    }

    pub fn expr(self) -> Expr<'a> {
        self.0.cast_last()
    }
}
