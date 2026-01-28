use crate::ast::AstNode;
use crate::fix::Fix;
use crate::kind::SyntaxKind;
use crate::set::SyntaxSet;
use crate::span::Span;
use compose_error_codes::ErrorCode;
use compose_utils::trace_log;
use ecow::{EcoString, EcoVec, eco_vec};
use std::fmt::{Debug, Formatter};
use std::ops::Range;
use std::sync::Arc;

/// A node in the untyped syntax tree.
///
/// A node can either be
/// - a leaf node representing a single token,
/// - an inner node containing one or more children
/// - or, an error node containing a syntax error.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct SyntaxNode(Repr);

// Constructors
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
            "{:?}{} ({:?}): {}",
            self.kind,
            if self.erroneous { " (err)" } else { "" },
            self.span,
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
    fn new(error: SyntaxError, text: impl Into<EcoString>) -> ErrorNode {
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
            "{} {:?}: {:?} ({})",
            match self.error.severity {
                SyntaxErrorSeverity::Error => "error",
                SyntaxErrorSeverity::Warning => "warning",
            },
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
    /// Additional notes to the user, indicating more details about the error.
    pub notes: EcoVec<EcoString>,
    /// A message to be displayed as a label for the error. This message is
    /// shown at the location of the error and can give more locational
    /// information
    pub label_message: Option<EcoString>,
    /// Related labels that provide additional information.
    pub labels: EcoVec<Label>,
    pub code: Option<&'static ErrorCode>,

    pub fixes: EcoVec<Fix>,

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
    pub fn with_message(&mut self, message: impl Into<EcoString>) -> &mut Self {
        self.message = message.into();
        self
    }

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

    pub fn with_fix(&mut self, fix: Fix) -> &mut Self {
        self.fixes.push(fix);
        self
    }
}

impl SyntaxError {
    pub(crate) fn new(message: impl Into<EcoString>, span: Span) -> SyntaxError {
        SyntaxError {
            span,
            message: message.into(),
            hints: eco_vec![],
            label_message: None,
            labels: eco_vec![],
            notes: eco_vec![],
            code: None,
            severity: SyntaxErrorSeverity::Error,
            fixes: eco_vec![],
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LinkedNode<'a> {
    node: &'a SyntaxNode,
    parent: Option<Arc<LinkedNode<'a>>>,
    /// The index of the node in the parent's children.
    index: usize,
    /// The byte offset of the node in the source.
    byte_offset: usize,
}

impl<'a> LinkedNode<'a> {
    pub fn new(node: &'a SyntaxNode) -> Self {
        Self {
            node,
            parent: None,
            index: 0,
            byte_offset: node.span().range().map(|r| r.start).unwrap_or(0),
        }
    }

    /// The node.
    pub fn node(&self) -> &'a SyntaxNode {
        self.node
    }

    /// The index of the node in the parent's children.
    pub fn index(&self) -> usize {
        self.index
    }

    /// The byte offset of the node in the source.
    pub fn byte_offset(&self) -> usize {
        self.byte_offset
    }

    pub fn span(&self) -> Span {
        self.node.span()
    }

    pub fn range(&self) -> Range<usize> {
        self.byte_offset..(self.byte_offset + self.node.len())
    }

    pub fn find(&self, span: Span) -> Option<LinkedNode<'a>> {
        let self_span = self.span();
        if self_span == span {
            return Some(self.clone());
        }

        let span_range = span.range()?;

        if let Some(self_range) = self_span.range() {
            // If this node's span is entirely after the span, it won't be in its children.
            if self_range.start > span_range.end {
                return None;
            }

            // if this node's span is entirely before the span, it won't be in its children.
            if self_range.end < span_range.start {
                return None;
            }
        }

        for child in self.children() {
            if let Some(found) = child.find(span) {
                return Some(found);
            }
        }

        None
    }
}

impl<'a> LinkedNode<'a> {
    pub fn children(&self) -> LinkedChildren<'a> {
        LinkedChildren {
            parent: Arc::new(self.clone()),
            iter: self.node.children().enumerate(),
            front: self.byte_offset,
            back: self.byte_offset + self.node.len(),
        }
    }

    pub fn parent(&self) -> Option<&LinkedNode<'a>> {
        self.parent.as_deref()
    }

    pub fn closest_parent(&self, kind: SyntaxKind) -> Option<&LinkedNode<'a>> {
        let mut node = self;
        while let Some(parent) = node.parent() {
            if parent.node().kind() == kind {
                return Some(parent);
            }
            node = parent;
        }
        None
    }

    pub fn closest_parent_any_of(&self, kind_set: SyntaxSet) -> Option<&LinkedNode<'a>> {
        let mut node = self;
        while let Some(parent) = node.parent() {
            if kind_set.contains(parent.node().kind()) {
                return Some(parent);
            }
            node = parent;
        }
        None
    }

    pub fn closest_parent_as<T: AstNode<'a>>(&self) -> Option<T> {
        let mut node = self;
        while let Some(parent) = node.parent() {
            if let Some(val) = T::from_untyped(parent.node()) {
                return Some(val);
            }
            node = parent;
        }

        None
    }

    pub fn first_child_of_kind(&self, kind: SyntaxKind) -> Option<LinkedNode<'a>> {
        self.children().find(|child| child.node().kind() == kind)
    }

    pub fn last_child_of_kind(&self, kind: SyntaxKind) -> Option<LinkedNode<'a>> {
        self.children()
            .rev()
            .find(|child| child.node().kind() == kind)
    }
}

/// An iterator over the children of a linked node.
pub struct LinkedChildren<'a> {
    parent: Arc<LinkedNode<'a>>,
    iter: std::iter::Enumerate<std::slice::Iter<'a, SyntaxNode>>,
    front: usize,
    back: usize,
}

impl<'a> Iterator for LinkedChildren<'a> {
    type Item = LinkedNode<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(index, node)| {
            let offset = self.front;
            self.front += node.len();
            LinkedNode {
                node,
                parent: Some(self.parent.clone()),
                index,
                byte_offset: offset,
            }
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl DoubleEndedIterator for LinkedChildren<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back().map(|(index, node)| {
            self.back -= node.len();
            LinkedNode {
                node,
                parent: Some(self.parent.clone()),
                index,
                byte_offset: self.back,
            }
        })
    }
}

impl ExactSizeIterator for LinkedChildren<'_> {}
