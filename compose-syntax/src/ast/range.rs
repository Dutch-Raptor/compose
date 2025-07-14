use crate::ast::macros::node;
use crate::ast::Expr;
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
    use super::*;
    use crate::assert_ast;
    use crate::ast::{AstNode, FieldAccess, FuncCall, Int, Parenthesized};
    use crate::test_utils::test_parse;

    #[test]
    fn test_range_exclusive() {
        assert_ast!(
            "1..5",
            range as Range {
                with start: Int = range.start().unwrap() => {
                    assert_eq!(start.get(), 1);
                }
                with end: Int = range.end().unwrap() => {
                    assert_eq!(end.get(), 5);
                }
                assert_eq!(range.is_inclusive(), false);
            }
        )
    }

    #[test]
    fn test_range_inclusive() {
        let nodes = test_parse("1..=5");
        let range: Range = nodes[0].cast().unwrap();

        assert_eq!(range.start().unwrap().to_text(), "1");
        assert_eq!(range.end().unwrap().to_text(), "5");
        assert!(range.is_inclusive());

        assert_ast!(
            "1..=5",
            range as Range {
                with start: Int = range.start().unwrap() => {
                    assert_eq!(start.get(), 1);
                }
                with end: Int = range.end().unwrap() => {
                    assert_eq!(end.get(), 5);
                }
                assert_eq!(range.is_inclusive(), true);
            }
        )
    }

    #[test]
    fn test_range_from() {
        assert_ast!(
            "1..",
            range as Range {
                with start: Int = range.start().unwrap() => {
                    assert_eq!(start.get(), 1);
                }
                assert_eq!(range.end().is_none(), true);
                assert_eq!(range.is_inclusive(), false);
            }
        )
    }

    #[test]
    fn test_range_from_inclusive() {
        assert_ast!(
            "1..=",
            range as Range {
                with start: Int = range.start().unwrap() => {
                    assert_eq!(start.get(), 1);
                }
                assert_eq!(range.end().is_none(), true);
                assert_eq!(range.is_inclusive(), true);
            }
        )
    }

    #[test]
    fn test_range_to_exclusive() {
        assert_ast!(
            "..5",
            range as Range {
                assert_eq!(range.start().is_none(), true);
                with end: Int = range.end().unwrap() => {
                    assert_eq!(end.get(), 5);
                }
                assert_eq!(range.is_inclusive(), false);
            }
        )
    }

    #[test]
    fn test_range_to_inclusive() {
        assert_ast!(
            "..=5",
            range as Range {
                assert_eq!(range.start().is_none(), true);
                with end: Int = range.end().unwrap() => {
                    assert_eq!(end.get(), 5);
                }
                assert_eq!(range.is_inclusive(), true);
            }
        )
    }

    #[test]
    fn test_range_full() {
        assert_ast!(
            "..",
            range as Range {
                assert_eq!(range.start().is_none(), true);
                assert_eq!(range.end().is_none(), true);
                assert_eq!(range.is_inclusive(), false);
            }
        )
    }

    #[test]
    fn test_range_with_expressions() {
        assert_ast!(
            "(1 + 2)..(3 * 4)",
            range as Range {
                with start: Parenthesized = range.start().unwrap() => {
                    assert_eq!(start.to_text(), "(1+2)");
                }
                with end: Parenthesized = range.end().unwrap() => {
                    assert_eq!(end.to_text(), "(3*4)");
                }
                assert_eq!(range.is_inclusive(), false);
            }
        )
    }

    #[test]
    fn test_range_with_method_calls() {
        assert_ast!(
            "x.min()..=y.max()",
            range as Range {
                with start: FuncCall = range.start().unwrap() => {
                    assert_eq!(start.to_text(), "x.min()");
                }
                with end: FuncCall = range.end().unwrap() => {
                    assert_eq!(end.to_text(), "y.max()");
                }
            }
        )
    }

    #[test]
    fn test_range_with_field_access() {
        assert_ast!(
            "point.x..point.y",
            range as Range {
                with start: FieldAccess = range.start().unwrap() => {
                    assert_eq!(start.to_text(), "point.x");
                }
                with end: FieldAccess = range.end().unwrap() => {
                    assert_eq!(end.to_text(), "point.y");
                }
            }
        )
    }

    #[test]
    fn test_nested_ranges() {
        let nodes = test_parse("(0..10)..20");
        let range: Range = nodes[0].cast().unwrap();

        assert_eq!(range.start().unwrap().to_text(), "(0..10)");
        assert_eq!(range.end().unwrap().to_text(), "20");
        assert!(!range.is_inclusive());

        assert_ast!(
            "(0..10)..20",
            range as Range {
                with start: Parenthesized = range.start().unwrap() => {
                    with inner_range: Range = start.expr() => {
                        with inner_start: Int = inner_range.start().unwrap() => {
                            assert_eq!(inner_start.get(), 0);
                        }
                        with inner_end: Int = inner_range.end().unwrap() => {
                            assert_eq!(inner_end.get(), 10);
                        }
                        assert_eq!(inner_range.is_inclusive(), false);
                    }
                }
                with end: Int = range.end().unwrap() => {
                    assert_eq!(end.get(), 20);
                }
                assert_eq!(range.is_inclusive(), false);
            }
        )
    }
}
