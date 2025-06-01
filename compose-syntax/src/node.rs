use crate::ast::AstNode;
use crate::kind::SyntaxKind;
use crate::span::Span;
use compose_error_codes::ErrorCode;
use compose_utils::trace_log;
use ecow::{EcoString, EcoVec, eco_vec};
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct SyntaxNode(Repr);

impl SyntaxNode {
    pub(crate) fn erroneous(&self) -> bool {
        match &self.0 {
            Repr::Leaf(_) => false,
            Repr::Inner(i) => i.erroneous,
            Repr::Error(e) => e.error.severity == SyntaxErrorSeverity::Error,
        }
    }
    pub fn errors(&self) -> Vec<SyntaxError> {
        if !self.erroneous() {
            return vec![];
        }

        if let Repr::Error(node) = &self.0 {
            vec![node.error.clone()]
        } else {
            self.children()
                .filter(|node| node.erroneous())
                .flat_map(|node| node.errors())
                .collect()
        }
    }

    pub fn warnings(&self) -> Vec<SyntaxError> {
        match &self.0 {
            Repr::Error(node) => {
                if node.error.severity == SyntaxErrorSeverity::Warning {
                    vec![node.error.clone()]
                } else {
                    vec![]
                }
            }
            Repr::Inner(i) => i.children.iter().flat_map(|node| node.warnings()).collect(),
            Repr::Leaf(_) => vec![],
        }
    }

    pub fn error_mut(&mut self) -> Option<&mut SyntaxError> {
        match &mut self.0 {
            Repr::Error(e) => Some(&mut Arc::make_mut(e).error),
            _ => None,
        }
    }

    pub(crate) fn descendents(&self) -> usize {
        match &self.0 {
            Repr::Leaf(_) | Repr::Error(_) => 1,
            Repr::Inner(i) => i.descendents,
        }
    }

    pub(crate) fn len(&self) -> usize {
        match &self.0 {
            Repr::Leaf(l) => l.len(),
            Repr::Inner(i) => i.len,
            Repr::Error(e) => e.len(),
        }
    }
}

// Converting from and to typed AST nodes.
impl SyntaxNode {
    pub fn is<'a, T: AstNode<'a>>(&'a self) -> bool {
        self.cast::<T>().is_some()
    }

    /// Tries to cast the node to type `T`. Returns None if it cannot.
    pub fn cast<'a, T: AstNode<'a>>(&'a self) -> Option<T> {
        T::from_untyped(self)
    }

    pub fn try_cast_first<'a, T: AstNode<'a>>(&'a self) -> Option<T> {
        self.children().find_map(Self::cast)
    }

    pub fn try_cast_last<'a, T: AstNode<'a>>(&'a self) -> Option<T> {
        self.children().rev().find_map(Self::cast)
    }

    pub(crate) fn cast_first<'a, T: AstNode<'a> + Default>(&'a self) -> T {
        self.try_cast_first().unwrap_or_default()
    }

    pub(crate) fn cast_last<'a, T: AstNode<'a> + Default>(&'a self) -> T {
        self.try_cast_last().unwrap_or_default()
    }
}

impl SyntaxNode {
    pub(crate) fn error(error: SyntaxError, text: impl Into<EcoString>) -> Self {
        Self(Repr::Error(Arc::new(ErrorNode::new(error, text))))
    }
    pub(crate) fn leaf(kind: SyntaxKind, text: impl Into<EcoString>, span: Span) -> Self {
        Self(Repr::Leaf(LeafNode::new(kind, text, span)))
    }

    pub(crate) fn inner(kind: SyntaxKind, children: Vec<SyntaxNode>) -> Self {
        Self(Repr::Inner(Arc::new(InnerNode::new(kind, children))))
    }

    pub(crate) const fn placeholder(kind: SyntaxKind) -> Self {
        if matches!(kind, SyntaxKind::Error) {
            panic!("cannot create error placeholder");
        }
        Self(Repr::Leaf(LeafNode {
            kind,
            text: EcoString::new(),
            span: Span::detached(),
        }))
    }

    pub fn kind(&self) -> SyntaxKind {
        match &self.0 {
            Repr::Leaf(l) => l.kind,
            Repr::Inner(i) => i.kind,
            Repr::Error(_) => SyntaxKind::Error,
        }
    }

    pub fn text(&self) -> &EcoString {
        static EMPTY: EcoString = EcoString::new();
        match &self.0 {
            Repr::Leaf(l) => &l.text,
            Repr::Inner(_) => &EMPTY,
            Repr::Error(e) => &e.text,
        }
    }

    /// Like text, but also returns the text of inner nodes.
    ///
    /// Builds a new string, so is more computationally expensive.
    pub fn to_text(&self) -> EcoString {
        let mut str = EcoString::new();

        fn inner(node: &SyntaxNode, str: &mut EcoString) {
            match &node.0 {
                Repr::Leaf(l) => str.push_str(&l.text),
                Repr::Inner(i) => {
                    for child in &i.children {
                        inner(child, str);
                    }
                }
                Repr::Error(e) => str.push_str(&e.text),
            }
        }

        inner(self, &mut str);
        str
    }

    pub fn children(&self) -> std::slice::Iter<'_, SyntaxNode> {
        match &self.0 {
            Repr::Leaf(_) | Repr::Error(_) => [].iter(),
            Repr::Inner(i) => i.children.iter(),
        }
    }

    /// Convert the node to another kind
    ///
    /// To convert to an error, use [Self::convert_to_error]
    pub(crate) fn convert_to_kind(&mut self, kind: SyntaxKind) {
        match &mut self.0 {
            Repr::Leaf(l) => l.kind = kind,
            Repr::Inner(i) => Arc::make_mut(i).kind = kind,
            Repr::Error(_) => panic!("cannot convert error node to kind"),
        }
    }

    /// Convert the node to an error with the same span and text
    pub(crate) fn convert_to_error(&mut self, message: impl Into<EcoString>) -> &mut SyntaxError {
        if self.kind() != SyntaxKind::Error {
            let span = self.span();
            let text = std::mem::take(self).into_text();
            *self = SyntaxNode::error(SyntaxError::new(message, span), text);
        }

        self.error_mut().unwrap()
    }

    fn into_text(self) -> EcoString {
        match self.0 {
            Repr::Leaf(l) => l.text,
            Repr::Inner(i) => i.children.iter().cloned().map(Self::into_text).collect(),
            Repr::Error(e) => e.text.clone(),
        }
    }

    pub(crate) fn expected(&mut self, expected: impl Into<EcoString>) {
        let kind = self.kind();
        let expected = expected.into();
        self.convert_to_error(format!("expected {}, found {:?}", &expected, kind));
        trace_log!("expected {}, found {:?}", &expected, kind);
    }

    pub fn span(&self) -> Span {
        match &self.0 {
            Repr::Leaf(l) => l.span,
            Repr::Inner(i) => i.span,
            Repr::Error(e) => e.error.span,
        }
    }
}

impl Default for SyntaxNode {
    fn default() -> Self {
        Self::leaf(SyntaxKind::End, EcoString::new(), Span::detached())
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
    pub(crate) fn len(&self) -> usize {
        self.text.len()
    }
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
    /// The number of descendents in this node (including itself).
    descendents: usize,
}

impl InnerNode {
    fn new(kind: SyntaxKind, children: Vec<SyntaxNode>) -> InnerNode {
        debug_assert!(kind != SyntaxKind::Error);

        let mut len = 0;
        let mut descendents = 1;
        let mut erroneous = false;

        for child in &children {
            len += child.len();
            descendents += child.descendents();
            erroneous |= child.erroneous();
        }

        let span = if children.is_empty() {
            Span::detached()
        } else {
            let first_span = children.first().unwrap().span();
            let last_span = children.last().unwrap().span();

            match (first_span.id(), first_span.range(), last_span.range()) {
                (Some(id), Some(start), Some(end)) => Span::new(id, start.start..end.end),
                (Some(id), Some(start), _) => Span::new(id, start),
                _ => Span::detached(),
            }
        };

        Self {
            kind,
            len,
            span,
            erroneous,
            children,
            descendents,
        }
    }
}

impl Debug for InnerNode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{:?}{}: {}",
            self.kind,
            if self.erroneous { " (err)" } else { "" },
            self.len
        )?;
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

    fn len(&self) -> usize {
        self.text.len()
    }
}

impl Debug for ErrorNode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "Error {:?}: {:?} ({})",
            self.error.span, self.text, self.error.message
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
    /// Additional notes to the user, indicating more details about the error.
    pub notes: EcoVec<EcoString>,
    /// A message to be displayed as a label for the error. This message is
    /// shown at the location of the error and can give more locational
    /// information
    pub label_message: Option<EcoString>,
    /// Related labels that provide additional information.
    pub labels: EcoVec<Label>,
    pub code: Option<&'static ErrorCode>,

    pub severity: SyntaxErrorSeverity,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum SyntaxErrorSeverity {
    Error,
    Warning,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Label {
    pub span: Span,
    pub message: EcoString,
    pub ty: LabelType,
}

impl Label {
    pub fn primary(span: Span, message: impl Into<EcoString>) -> Label {
        Label {
            span,
            message: message.into(),
            ty: LabelType::Primary,
        }
    }

    pub fn secondary(span: Span, message: impl Into<EcoString>) -> Label {
        Label {
            span,
            message: message.into(),
            ty: LabelType::Secondary,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum LabelType {
    Primary,
    Secondary,
}

impl SyntaxError {
    pub fn with_note(&mut self, note: impl Into<EcoString>) -> &mut Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_hint(&mut self, hint: impl Into<EcoString>) -> &mut Self {
        self.hints.push(hint.into());
        self
    }

    pub fn with_label_message(&mut self, message: impl Into<EcoString>) -> &mut Self {
        self.label_message = Some(message.into());
        self
    }

    pub fn with_label(&mut self, label: Label) -> &mut Self {
        self.labels.push(label);
        self
    }

    pub fn with_code(&mut self, code: &'static ErrorCode) -> &mut Self {
        self.code = Some(code);
        self
    }

    pub fn with_severity(&mut self, severity: SyntaxErrorSeverity) -> &mut Self {
        self.severity = severity;
        self
    }
}

impl SyntaxError {
    pub(crate) fn new(message: impl Into<EcoString> + Sized, span: Span) -> SyntaxError {
        SyntaxError {
            span,
            message: message.into(),
            hints: eco_vec![],
            label_message: None,
            labels: eco_vec![],
            notes: eco_vec![],
            code: None,
            severity: SyntaxErrorSeverity::Error,
        }
    }
}
