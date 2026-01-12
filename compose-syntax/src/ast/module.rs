use crate::ast::macros::node;
use crate::ast::{AstNode, Expr, Ident, Str};
use crate::{SyntaxKind, SyntaxNode, ast, Span};
use ecow::EcoString;

node! {
    struct ModuleImport
}

impl<'a> ModuleImport<'a> {
    pub fn source(self) -> EcoString {
        self.0.cast_first::<Str>().get()
    }

    pub fn source_span(self) -> Span {
        self.0.cast_first::<Str>().span()
    }

    pub fn alias(self) -> Option<ast::Ident<'a>> {
        let mut children = self
            .0
            .children()
            .take_while(|n| n.kind() != SyntaxKind::Colon);
        while let Some(child) = children.next() {
            if child.kind() == SyntaxKind::AsKW {
                return children.next().and_then(SyntaxNode::cast);
            }
        }

        None
    }

    pub fn items(self) -> impl DoubleEndedIterator<Item = ImportItem<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

node! {
    struct ImportItem
}

impl<'a> ImportItem<'a> {
    pub fn path(self) -> Expr<'a> {
        self.0.cast_first()
    }

    pub fn alias(self) -> Option<Ident<'a>> {
        self.0
            .children()
            .skip_while(|n| n.kind() != SyntaxKind::AsKW)
            .find_map(SyntaxNode::cast)
    }
}

#[cfg(test)]
mod tests {
    use crate::assert_ast;
    use super::*;

    #[test]
    fn simple_module_import() {
        assert_ast! {
            "import \"foo\";",
            module as ModuleImport {
                assert_eq!(module.source(), "foo");
                assert_eq!(module.alias(), None);
                assert_eq!(module.items().count(), 0);
            }
        }
    }

    #[test]
    fn module_with_alias() {
        assert_ast! {
            "import \"foo\" as bar;",
            module as ModuleImport {
                assert_eq!(module.source(), "foo");
                assert_eq!(module.items().count(), 0);
                with alias: Ident = module.alias().unwrap() => {
                    assert_eq!(alias.get(), "bar");
                }
            }
        }
    }

    #[test]
    fn module_with_items() {
        assert_ast! {
            "import \"foo\" as bar { baz, quz as quux };",
            module as ModuleImport {
                assert_eq!(module.source(), "foo");
                module.items() => [
                    item as ImportItem {
                        with path: Ident = item.path() => {
                            assert_eq!(path.get(), "baz");
                        }
                        assert_eq!(item.alias(), None);
                    }
                    item as ImportItem {
                        with path: Ident = item.path() => {
                            assert_eq!(path.get(), "quz");
                        }
                        assert_eq!(item.alias().unwrap().get(), "quux");
                    }
                ]
            }
        }
    }
}
