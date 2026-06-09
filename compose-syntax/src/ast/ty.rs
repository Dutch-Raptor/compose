use crate::SyntaxNode;
use crate::ast::Ident;
use crate::ast::macros::node;

node! {
    struct Type
}

impl<'a> Type<'a> {
    pub fn ident(self) -> Ident<'a> {
        self.0.cast_first()
    }

    pub fn args(self) -> Option<TypeArgs<'a>> {
        self.0.try_cast_last()
    }
}

node! {
    struct TypeArgs
}

impl<'a> TypeArgs<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item = Type<'a>> {
        self.0.children().filter_map(SyntaxNode::cast)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_ast;
    use crate::ast::LetBinding;

    #[test]
    fn type_annotation() {
        assert_ast!(
            "let xs: Map<Str, Array<Int>> = []",
            binding as LetBinding {
                with ty: Type = binding.type_annotation().unwrap() => {
                    with ident: Ident = ty.ident() => {
                        assert_eq!(ident.get(), "Map");
                    }
                    ty.args().unwrap().items() => [
                        arg as Type {
                            with ident: Ident = arg.ident() => {
                                assert_eq!(ident.get(), "Str");
                            }
                            assert!(arg.args().is_none());
                        }
                        arg as Type {
                            with ident: Ident = arg.ident() => {
                                assert_eq!(ident.get(), "Array");
                            }
                            arg.args().unwrap().items() => [
                                inner as Type {
                                    with ident: Ident = inner.ident() => {
                                        assert_eq!(ident.get(), "Int");
                                    }
                                    assert!(inner.args().is_none());
                                }
                            ]
                        }
                    ]
                }
            }
        )
    }
}
