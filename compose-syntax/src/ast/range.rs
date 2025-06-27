use crate::ast::Expr;
use crate::ast::macros::node;
use crate::{SyntaxKind, SyntaxNode};

node! {
    struct Range
}

impl<'a> Range<'a> {
    pub fn start(self) -> Option<Expr<'a>> {
        self.0
            .children()
            .take_while(|p| !matches!(p.kind(), SyntaxKind::Dots | SyntaxKind::DotsEq))
            .find_map(SyntaxNode::cast)
    }

    pub fn end(self) -> Option<Expr<'a>> {
        let mut children = self.0.children();
        // move past the range operator
        while let Some(p) = children.next() {
            if matches!(p.kind(), SyntaxKind::Dots | SyntaxKind::DotsEq) {
                break;
            }
        }
        
        children.find_map(SyntaxNode::cast)
    }

    pub fn is_inclusive(self) -> bool {
        self.0.children().any(|p| p.kind() == SyntaxKind::DotsEq)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::AstNode;
    use super::*;
    use crate::test_utils::test_parse;

    #[test]
    fn test_range_exclusive() {
        let nodes = test_parse("1..5");
        let range: Range = nodes[0].cast().unwrap();

        assert_eq!(range.start().unwrap().to_text(), "1");
        assert_eq!(range.end().unwrap().to_text(), "5");
        assert!(!range.is_inclusive());
    }

    #[test]
    fn test_range_inclusive() {
        let nodes = test_parse("1..=5");
        let range: Range = nodes[0].cast().unwrap();

        assert_eq!(range.start().unwrap().to_text(), "1");
        assert_eq!(range.end().unwrap().to_text(), "5");
        assert!(range.is_inclusive());
    }

    #[test]
    fn test_range_from() {
        let nodes = test_parse("1..");
        let range: Range = nodes[0].cast().unwrap();

        assert_eq!(range.start().unwrap().to_text(), "1");
        assert!(range.end().is_none());
        assert!(!range.is_inclusive());
    }

    #[test]
    fn test_range_from_inclusive() {
        let nodes = test_parse("1..=");
        let range: Range = nodes[0].cast().unwrap();

        assert_eq!(range.start().unwrap().to_text(), "1");
        assert!(range.end().is_none());
        assert!(range.is_inclusive());
    }

    #[test]
    fn test_range_to_exclusive() {
        let nodes = test_parse("..5");
        let range: Range = nodes[0].cast().unwrap();

        assert!(range.start().is_none());
        assert_eq!(range.end().unwrap().to_text(), "5");
        assert!(!range.is_inclusive());
    }

    #[test]
    fn test_range_to_inclusive() {
        let nodes = test_parse("..=5");
        let range: Range = nodes[0].cast().unwrap();

        assert!(range.start().is_none());
        assert_eq!(range.end().unwrap().to_text(), "5");
        assert!(range.is_inclusive());
    }

    #[test]
    fn test_range_full() {
        let nodes = test_parse("..");
        let range: Range = nodes[0].cast().unwrap();

        assert!(range.start().is_none());
        assert!(range.end().is_none());
        assert!(!range.is_inclusive());
    }

    #[test]
    fn test_range_with_expressions() {
        let nodes = test_parse("(1 + 2)..(3 * 4)");
        let range: Range = nodes[0].cast().unwrap();

        assert_eq!(range.start().unwrap().to_text(), "(1+2)");
        assert_eq!(range.end().unwrap().to_text(), "(3*4)");
        assert!(!range.is_inclusive());
    }

    #[test]
    fn test_range_with_method_calls() {
        let nodes = test_parse("x.min()..=y.max()");
        let range: Range = nodes[0].cast().unwrap();

        assert_eq!(range.start().unwrap().to_text(), "x.min()");
        assert_eq!(range.end().unwrap().to_text(), "y.max()");
        assert!(range.is_inclusive());
    }

    #[test]
    fn test_range_with_field_access() {
        let nodes = test_parse("point.x..point.y");
        let range: Range = nodes[0].cast().unwrap();

        assert_eq!(range.start().unwrap().to_text(), "point.x");
        assert_eq!(range.end().unwrap().to_text(), "point.y");
        assert!(!range.is_inclusive());
    }

    #[test]
    fn test_nested_ranges() {
        let nodes = test_parse("(0..10)..20");
        let range: Range = nodes[0].cast().unwrap();

        assert_eq!(range.start().unwrap().to_text(), "(0..10)");
        assert_eq!(range.end().unwrap().to_text(), "20");
        assert!(!range.is_inclusive());
    }
}
