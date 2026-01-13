use crate::SyntaxNode;
use crate::ast::Expr;
use crate::ast::macros::node;

node! {
    struct MapLiteral
}

impl<'a> MapLiteral<'a> {
    pub fn entries(self) -> impl DoubleEndedIterator<Item = MapEntry<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

node! {
    struct MapEntry
}

impl<'a> MapEntry<'a> {
    pub fn key(self) -> Expr<'a> {
        self.0.cast_first()
    }

    pub fn value(self) -> Expr<'a> {
        self.0.cast_last()
    }
}


#[cfg(test)]
mod tests {
    use crate::ast::Str;
    use crate::assert_ast;
    use crate::ast::{Ident, Int, MapEntry, MapLiteral};

    #[test]
    fn test_map_with_identifier_keys() {
        assert_ast! {
            "{ a: 1, b: 2 }",
            map as MapLiteral {
                map.entries() => [
                    entry as MapEntry {
                        with i: Ident = entry.key() => {
                            assert_eq!(i.get(), "a");
                        }
                        with i: Int = entry.value() => {
                            assert_eq!(i.get(), 1);
                        }
                    }
                    entry as MapEntry {
                        with i: Ident = entry.key() => {
                            assert_eq!(i.get(), "b");
                        }
                        with i: Int = entry.value() => {
                            assert_eq!(i.get(), 2);
                        }
                    }
                ]
            }
        }
    }

    #[test]
    fn test_map_with_string_keys() {
        assert_ast! {
            "{ \"a\": 1, \"b\": 2 }",
            map as MapLiteral {
                map.entries() => [
                    entry as MapEntry {
                        with k: Str = entry.key() => {
                            assert_eq!(k.get(), "a");
                        }
                        with v: Int = entry.value() => {
                            assert_eq!(v.get(), 1);
                        }
                    }
                    entry as MapEntry {
                        with k: Str = entry.key() => {
                            assert_eq!(k.get(), "b");
                        }
                        with v: Int = entry.value() => {
                            assert_eq!(v.get(), 2);
                        }
                    }
                ]
            }
        }
    }

    #[test]
    fn test_map_with_shorthand_keys() {
        assert_ast! {
            "{ a:, b, }",
            map as MapLiteral {
                map.entries() => [
                    entry as MapEntry {
                        with k: Ident = entry.key() => {
                            assert_eq!(k.get(), "a");
                        }
                        with v: Ident = entry.value() => {
                            assert_eq!(v.get(), "a");
                        }
                    }
                    entry as MapEntry {
                        with k: Ident = entry.key() => {
                            assert_eq!(k.get(), "b");
                        }
                        with v: Ident = entry.value() => {
                            assert_eq!(v.get(), "b");
                        }
                    }
                ]
            }
        }
    }
}
