/// Implements [`AstNode`] for a struct whose name matches a [`SyntaxKind`]
/// variant.
///
/// The struct becomes a wrapper around a [`SyntaxNode`] pointer, and the
/// implementation of [`AstNode::from_untyped`] checks that the pointer's kind
/// matches when converting, returning `Some` or `None` respectively.
///
/// The generated struct is the basis for typed accessor methods for properties
/// of this AST node.
macro_rules! node {
    ($(#[$attr:meta])* struct $name:ident) => {
        // Create the struct as a wrapper around a `SyntaxNode` reference.
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        #[repr(transparent)]
        $(#[$attr])*
        pub struct $name<'a>(&'a $crate::SyntaxNode);

        impl<'a> $crate::ast::AstNode<'a> for $name<'a> {
            #[inline]
            fn from_untyped(node: &'a $crate::SyntaxNode) -> Option<Self> {
                if node.kind() == $crate::kind::SyntaxKind::$name {
                    Some(Self(node))
                } else {
                    Option::None
                }
            }

            #[inline]
            fn to_untyped(&self) -> &'a $crate::SyntaxNode {
                self.0
            }
        }

        impl Default for $name<'_> {
            #[inline]
            fn default() -> Self {
                static PLACEHOLDER: $crate::SyntaxNode
                    = $crate::SyntaxNode::placeholder($crate::kind::SyntaxKind::$name);
                Self(&PLACEHOLDER)
            }
        }
    };
}

pub(crate) use node;
