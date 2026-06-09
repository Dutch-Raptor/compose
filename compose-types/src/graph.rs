use crate::ty::{LiteralKind, Ty, TypeVar};
use compose_resolve::SymbolId;
use compose_syntax::Span;
use std::collections::{HashMap, HashSet};

// ── Inference Reasons ─────────────────────────────────────────────────────────

/// Human-readable explanation for a single inference step.
#[derive(Debug, Clone)]
pub enum InferenceReason {
    /// `Vec::new()` — constructed with unknown element type.
    Construction { method: SymbolId },
    /// `v.push(1)` — argument constrained the generic parameter.
    MethodArgument { method: SymbolId, arg_index: usize },
    /// `let x: T = ...` — explicit annotation.
    Annotation,
    /// `x = expr` — a later assignment constrained an existing binding.
    Assignment { target_span: Span },
    /// `fn foo(x: int)` — function parameter declaration.
    FunctionParam {
        fn_name: SymbolId,
        param_name: SymbolId,
    },
    /// Return type of a function call constrained a variable.
    CallReturn { fn_name: SymbolId },
    /// `expr;` — the value was discarded by a semicolon.
    UnitCoercion { expr_span: Span },
    /// Literal defaulted to its natural type (e.g. 1 → int).
    LiteralDefault { kind: LiteralKind },
    /// Two if/match branches unified.
    BranchUnification { other_branch: Span },
    /// Binary operator constrained the operand.
    BinaryOp { op_span: Span },
}

// ── Inference Steps ───────────────────────────────────────────────────────────

/// A single entry in the inference history of a type variable:
/// what was learned, where, and why.
#[derive(Debug, Clone)]
pub struct InferenceStep {
    /// What the variable was narrowed to at this step.
    pub ty: Ty,
    /// Where in source code caused this narrowing.
    pub span: Span,
    /// Human-readable cause.
    pub reason: InferenceReason,
}

// ── Inference Edges ───────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum EdgeKind {
    /// This variable was bound to a concrete type.
    /// Added during unification when bind_with_evidence() is called.
    BoundTo { ty: Ty, step: InferenceStep },

    /// This variable was linked to another variable by an equality constraint.
    Related { step: InferenceStep },

    /// This variable's value was discarded by a semicolon.
    /// Added during unit-coercion solving.
    Discarded {
        expr_ty: Ty,
        expr_span: Span,
        semi_span: Span,
    },
}

#[derive(Debug, Clone)]
pub struct InferenceEdge {
    pub from: TypeVar,
    pub kind: EdgeKind,
}

// ── Inference Graph ───────────────────────────────────────────────────────────

/// Records the full provenance of every type variable across the entire
/// type-checking pass. Used exclusively for diagnostic reporting — the
/// actual type-checking uses Substitution.
#[derive(Debug, Default)]
pub struct InferenceGraph {
    edges: Vec<InferenceEdge>,
    /// Where each variable was first introduced (the "birth" step).
    origins: HashMap<TypeVar, InferenceStep>,
}

impl InferenceGraph {
    // ── Writing ──────────────────────────────────────────────────────────────

    /// Record that `var` was introduced at this step.
    /// Called when a fresh TypeVar is created and immediately given meaning.
    pub fn record_origin(&mut self, var: TypeVar, step: InferenceStep) {
        self.origins.entry(var).or_insert(step);
    }

    /// Record that `var` was bound to a concrete type.
    /// Called from Substitution::bind_with_evidence().
    pub fn record_binding(&mut self, var: TypeVar, ty: Ty, step: InferenceStep) {
        self.edges.push(InferenceEdge {
            from: var,
            kind: EdgeKind::BoundTo { ty, step },
        });
    }

    /// Record that `var` was unified with another variable because of a source
    /// construct such as an assignment.
    pub fn record_relation(&mut self, var: TypeVar, step: InferenceStep) {
        self.edges.push(InferenceEdge {
            from: var,
            kind: EdgeKind::Related { step },
        });
    }

    /// Record that a variable's value was discarded by a semicolon.
    /// Called during unit-coercion solving when a concrete type is discarded.
    pub fn record_discard(
        &mut self,
        block_var: TypeVar,
        expr_ty: Ty,
        expr_span: Span,
        semi_span: Span,
    ) {
        self.edges.push(InferenceEdge {
            from: block_var,
            kind: EdgeKind::Discarded {
                expr_ty,
                expr_span,
                semi_span,
            },
        });
    }

    // ── Reading ───────────────────────────────────────────────────────────────

    /// Find the UnitCoercion/Discard edge for this block variable, if any.
    /// Used during error rendering to explain why a block resolved to Unit.
    pub fn discard_for(&self, var: TypeVar) -> Option<&EdgeKind> {
        self.edges.iter().find_map(|e| {
            if e.from == var {
                if let EdgeKind::Discarded { .. } = &e.kind {
                    return Some(&e.kind);
                }
            }
            None
        })
    }

    /// Collect the full chronological inference trail for a variable.
    ///
    /// Walks direct narrowings and equality relations. The result is sorted by
    /// source position.
    pub fn collect_trail(&self, start_var: TypeVar) -> Vec<TrailEntry> {
        let mut trail = Vec::new();
        let mut visited = HashSet::new();
        self.collect_trail_inner(start_var, &mut trail, &mut visited);
        trail.sort_by_key(|e| e.step.span.range().unwrap_or_default().start);
        trail
    }

    fn collect_trail_inner(
        &self,
        var: TypeVar,
        trail: &mut Vec<TrailEntry>,
        visited: &mut HashSet<TypeVar>,
    ) {
        if !visited.insert(var) {
            return; // cycle guard
        }

        // Include the origin step if this var has one and no edges yet explain it.
        if let Some(origin) = self.origins.get(&var) {
            trail.push(TrailEntry {
                var,
                step: origin.clone(),
                role: TrailRole::Introduction,
            });
        }

        for edge in self.edges.iter().filter(|e| e.from == var) {
            match &edge.kind {
                EdgeKind::BoundTo { ty, step } => {
                    trail.push(TrailEntry {
                        var,
                        step: step.clone(),
                        role: TrailRole::Narrowing { ty: ty.clone() },
                    });
                }

                EdgeKind::Related { step } => {
                    trail.push(TrailEntry {
                        var,
                        step: step.clone(),
                        role: TrailRole::Relation,
                    });
                }

                // Discard edges are handled separately via discard_for().
                EdgeKind::Discarded { .. } => {}
            }
        }
    }

    /// Collect trails for every TypeVar mentioned in a type.
    pub fn collect_trails_for_ty(&self, ty: &Ty) -> Vec<TrailEntry> {
        let mut trail = Vec::new();
        let mut visited = HashSet::new();
        for var in ty.free_vars() {
            self.collect_trail_inner(var, &mut trail, &mut visited);
        }
        trail.sort_by_key(|e| e.step.span.range().unwrap_or_default().start);
        trail
    }

    /// Transfer all edges and origins from `from_var` onto `to_var`.
    /// Called during unification when two variables are merged (Var ~ Var).
    pub fn merge_var_histories(&mut self, from_var: TypeVar, to_var: TypeVar) {
        // Re-key all edges from `from_var` to `to_var`.
        for edge in self.edges.iter_mut() {
            if edge.from == from_var {
                edge.from = to_var;
            }
        }
        // Transfer origin if `to_var` doesn't have one yet.
        if let Some(origin) = self.origins.remove(&from_var) {
            self.origins.entry(to_var).or_insert(origin);
        }
    }
}

// ── Trail Entries ─────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct TrailEntry {
    pub var: TypeVar,
    pub step: InferenceStep,
    pub role: TrailRole,
}

#[derive(Debug, Clone)]
pub enum TrailRole {
    /// The variable was first introduced here.
    Introduction,
    /// The variable was bound to a concrete type.
    Narrowing { ty: Ty },
    /// The variable was related to another variable by a source construct.
    Relation,
}
