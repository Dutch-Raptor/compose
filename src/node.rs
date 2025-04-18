use crate::kind::SyntaxKind;
use crate::span::Span;
use ecow::{EcoString, EcoVec, eco_vec};
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct SyntaxNode(Repr);

impl SyntaxNode {
    pub(crate) fn error(error: SyntaxError, text: impl Into<EcoString>) -> Self {
        Self(Repr::Error(Arc::new(ErrorNode::new(error, text))))
    }
    pub(crate) fn leaf(kind: SyntaxKind, text: impl Into<EcoString>, span: Span) -> Self {
        Self(Repr::Leaf(LeafNode::new(kind, text, span)))
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
enum Repr {
    Leaf(LeafNode),
    Inner(Arc<InnerNode>),
    Error(Arc<ErrorNode>),
}

impl Debug for SyntaxNode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match &self.0 {
            Repr::Leaf(leaf) => leaf.fmt(f),
            Repr::Inner(inner) => inner.fmt(f),
            Repr::Error(node) => node.fmt(f),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct LeafNode {
    /// The kind of the node
    kind: SyntaxKind,
    /// The span of the node
    span: Span,
    /// The source text of the node
    text: EcoString,
}

impl LeafNode {
    fn new(kind: SyntaxKind, text: impl Into<EcoString>, span: Span) -> LeafNode {
        Self {
            kind,
            span,
            text: text.into(),
        }
    }
}

impl Debug for LeafNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} ({}): {:?}",
            self.kind,
            match self.span.range() {
                None => "".to_string(),
                Some(r) => format!("{r:?}"),
            },
            self.text
        )
    }
}

/// An inner node in the untyped syntax tree.
#[derive(Clone, Eq, PartialEq, Hash)]
struct InnerNode {
    /// What kind of node this is (each kind would have its own struct in a
    /// strongly typed AST).
    kind: SyntaxKind,
    /// The byte length of the node in the source.
    len: usize,
    /// The node's span.
    span: Span,
    /// Whether this node or any of its children are erroneous.
    erroneous: bool,
    /// This node's children, losslessly make up this node.
    children: Vec<SyntaxNode>,
}

impl Debug for InnerNode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:?}: {}", self.kind, self.len)?;
        if !self.children.is_empty() {
            f.write_str(" ")?;
            f.debug_list().entries(&self.children).finish()?;
        }
        Ok(())
    }
}

/// An error node in the untyped syntax tree.
#[derive(Clone, Eq, PartialEq, Hash)]
struct ErrorNode {
    /// The source text of the node.
    text: EcoString,
    /// The syntax error.
    error: SyntaxError,
}

impl ErrorNode {
    fn new(error: SyntaxError, text: impl Into<EcoString> + Sized) -> ErrorNode {
        Self {
            text: text.into(),
            error,
        }
    }
}

impl Debug for ErrorNode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "Error {:?}: {:?} ({})",
            self.error.span,
            self.text,
            self.error.message
        )
    }
}

/// A syntactical error.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SyntaxError {
    /// The node's span.
    pub span: Span,
    /// The error message.
    pub message: EcoString,
    /// Additional hints to the user, indicating how this error could be avoided
    /// or worked around.
    pub hints: EcoVec<EcoString>,
}

impl SyntaxError {
    pub(crate) fn new(message: impl Into<EcoString> + Sized, span: Span) -> SyntaxError {
        SyntaxError {
            span,
            message: message.into(),
            hints: eco_vec![],
        }
    }
}
