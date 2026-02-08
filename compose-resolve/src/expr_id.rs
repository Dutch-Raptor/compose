use compose_syntax::{Span, SyntaxNode};
use indexmap::IndexMap;
use std::num::NonZeroU64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(NonZeroU64);

impl ExprId {
    /// Creates a new instance of the struct using the provided `id`.
    ///
    /// # Parameters
    /// - `id`: A `u64` value to initialise the struct. Must be a non-zero value.
    ///
    /// # Returns
    /// Returns a new instance of the struct if the provided `id` is valid.
    ///
    /// # Panics
    /// This function will panic if `id` is zero, as it attempts to create a
    /// `NonZeroU64` which requires a non-zero input.
    pub fn new(id: u64) -> Self {
        Self(NonZeroU64::new(id).expect("ExprId must be non-zero"))
    }

    pub fn next(&self) -> Self {
        Self(self.0.saturating_add(1))
    }
}

/// maps from Spans to ExprIds and vice versa
pub struct SpanExprIdTable {
    span_to_expr_id: IndexMap<Span, ExprId>,
    expr_id_to_span: IndexMap<ExprId, Span>,
    next_expr_id: ExprId,
}

impl SpanExprIdTable {
    pub fn new() -> Self {
        Self {
            span_to_expr_id: IndexMap::new(),
            expr_id_to_span: IndexMap::new(),
            next_expr_id: ExprId::new(1),
        }
    }

    pub fn get_or_insert(&mut self, span: Span) -> ExprId {
        self.span_to_expr_id
            .entry(span)
            .or_insert_with(|| {
                let expr_id = self.next_expr_id;
                self.next_expr_id = self.next_expr_id.next();
                self.expr_id_to_span.insert(expr_id, span);
                expr_id
            })
            .clone()
    }

    pub fn get_span(&self, expr_id: ExprId) -> Option<Span> {
        self.expr_id_to_span.get(&expr_id).copied()
    }

    pub fn get_expr_id(&self, span: Span) -> Option<ExprId> {
        self.span_to_expr_id.get(&span).copied()
    }
}

impl SpanExprIdTable {
    pub fn visit_node(&mut self, node: &SyntaxNode) {
        self.get_or_insert(node.span());
        for child in node.children() {
            self.visit_node(child);
        }
    }
}
