use crate::{
    bounds::BoundEntry,
    constraint::{BoundOrigin, ConstraintOrigin},
    graph::{EdgeKind, InferenceReason, TrailEntry},
    intern::InterfaceName,
    namer::VarNamer,
    subst::Substitution,
    ty::{LiteralKind, Ty, TypeVar},
    InferenceGraph,
};
use compose_syntax::Span;

// ── Type Errors ───────────────────────────────────────────────────────────────

#[derive(Debug)]
pub enum TypeError {
    /// Two types could not be unified.
    Mismatch {
        expected: Ty,
        found: Ty,
        origin: ConstraintOrigin,
        span: Span,
        /// The chronological trail of inference steps that led to the conflict.
        /// Used to emit "inferred as X here" notes.
        inference_trail: Vec<TrailEntry>,
    },

    /// A type variable appears in its own binding (e.g. T = Vec<T>).
    InfiniteType { var: TypeVar, ty: Ty, span: Span },

    /// A type variable was never resolved — the user must annotate.
    AmbiguousType { var: TypeVar, span: Span },

    /// A literal was used in a context that doesn't accept that literal kind.
    /// e.g. `let x: bool = 1`
    LiteralTypeMismatch {
        kind: LiteralKind,
        expected: Ty,
        found: Ty,
        span: Span,
        inference_trail: Vec<TrailEntry>,
    },

    /// A concrete type does not implement a required interface.
    BoundNotSatisfied {
        ty: Ty,
        interface: InterfaceName,
        interface_args: Vec<Ty>,
        origin: BoundOrigin,
        span: Span,
    },

    /// A method/operation inside a generic function requires a bound
    /// not declared in the function's signature.
    MissingBoundInSignature {
        /// The type parameter variable.
        type_param: TypeVar,
        /// The interface that was required but not declared.
        required: InterfaceName,
        origin: BoundOrigin,
        span: Span,
        /// The bounds that *were* declared — shown in the diagnostic.
        declared_bounds: Vec<BoundEntry>,
    },
}

// ── Diagnostic Rendering ──────────────────────────────────────────────────────
//
// This module renders TypeErrors into human-readable diagnostic structs.
// In a real compiler you would integrate with codespan-reporting or similar.

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub labels: Vec<DiagLabel>,
    pub notes: Vec<String>,
    pub suggestion: Option<Suggestion>,
}

#[derive(Debug)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug)]
pub struct DiagLabel {
    pub span: Span,
    pub message: String,
    pub primary: bool,
}

#[derive(Debug)]
pub struct Suggestion {
    pub span: Span,
    pub message: String,
    /// Replacement text (empty string = delete).
    pub replacement: String,
}

impl Diagnostic {
    fn error(message: impl Into<String>) -> Self {
        Diagnostic {
            severity: Severity::Error,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
            suggestion: None,
        }
    }

    fn primary(mut self, span: Span, message: impl Into<String>) -> Self {
        self.labels.push(DiagLabel {
            span,
            message: message.into(),
            primary: true,
        });
        self
    }

    fn secondary(mut self, span: Span, message: impl Into<String>) -> Self {
        self.labels.push(DiagLabel {
            span,
            message: message.into(),
            primary: false,
        });
        self
    }

    fn note(mut self, message: impl Into<String>) -> Self {
        self.notes.push(message.into());
        self
    }

    fn suggest(
        mut self,
        span: Span,
        message: impl Into<String>,
        replacement: impl Into<String>,
    ) -> Self {
        self.suggestion = Some(Suggestion {
            span,
            message: message.into(),
            replacement: replacement.into(),
        });
        self
    }
}

// ── Renderer ─────────────────────────────────────────────────────────────────

pub struct DiagnosticRenderer<'a> {
    pub graph: &'a InferenceGraph,
    pub namer: &'a VarNamer,
    pub subst: &'a mut Substitution,
}

impl<'a> DiagnosticRenderer<'a> {
    pub fn render(&mut self, error: &TypeError) -> Diagnostic {
        match error {
            TypeError::Mismatch {
                expected,
                found,
                origin,
                span,
                inference_trail,
            } => self.render_mismatch(expected, found, origin, *span, inference_trail),
            TypeError::InfiniteType { var, ty, span } => Diagnostic::error(format!(
                "infinite type: `{}` would contain itself as `{}`",
                var, ty
            ))
            .primary(*span, "recursive type here"),
            TypeError::AmbiguousType { var, span } => {
                let name = self.namer.name_for(*var);
                Diagnostic::error(format!("type of `{name}` is ambiguous"))
                    .primary(*span, "cannot infer type — add an annotation")
            }
            TypeError::LiteralTypeMismatch {
                kind,
                expected,
                found,
                span,
                inference_trail,
            } => self.render_literal_mismatch(kind, expected, found, *span, inference_trail),
            TypeError::BoundNotSatisfied {
                ty,
                interface,
                origin,
                span,
                ..
            } => self.render_bound_not_satisfied(ty, interface, origin, *span),
            TypeError::MissingBoundInSignature {
                type_param,
                required,
                origin,
                span,
                declared_bounds,
            } => self.render_missing_bound(type_param, required, origin, *span, declared_bounds),
        }
    }

    fn render_mismatch(
        &mut self,
        expected: &Ty,
        found: &Ty,
        origin: &ConstraintOrigin,
        span: Span,
        trail: &[TrailEntry],
    ) -> Diagnostic {
        let primary_message = match origin {
            ConstraintOrigin::Assignment { .. } => {
                format!("this assignment gives `{found}`, but `{expected}` was expected")
            }
            _ => format!("expected `{expected}`, found `{found}`"),
        };

        let mut diag = Diagnostic::error(format!(
            "mismatched types: expected `{expected}`, found `{found}`"
        ))
        .primary(span, primary_message);

        // ── Origin-specific context ───────────────────────────────────────────

        match origin {
            ConstraintOrigin::ReturnType { fn_span, .. } => {
                diag = diag.secondary(
                    *fn_span,
                    format!("function declared to return `{expected}` here"),
                );
            }
            ConstraintOrigin::BranchArm {
                first_arm_span,
                this_arm_span,
            } => {
                diag = diag
                    .secondary(
                        *first_arm_span,
                        format!("first branch has type `{expected}`"),
                    )
                    .secondary(*this_arm_span, format!("this branch has type `{found}`"));
            }
            ConstraintOrigin::FunctionArg {
                call_span,
                param_index,
                ..
            } => {
                diag = diag.secondary(
                    *call_span,
                    format!("argument {param_index} expected to be `{expected}`"),
                );
            }
            ConstraintOrigin::Assignment { target_span, .. } => {
                diag = diag.secondary(*target_span, format!("this place expects `{expected}`"));
            }
            _ => {}
        }

        // ── UnitCoercion special case ─────────────────────────────────────────
        //
        // If `found` is Unit, check whether a semicolon caused it.
        // Then check if the discarded type matches `expected` — if so,
        // emit the targeted "remove the semicolon" suggestion.

        if *found == Ty::Unit {
            // Find the block var that resolved to Unit.
            let block_var = found.free_vars().into_iter().next().or_else(|| {
                // found is concrete Unit — look up the block var from trail
                trail.iter().find_map(|e| {
                    if matches!(e.role, crate::graph::TrailRole::Introduction) {
                        Some(e.var)
                    } else {
                        None
                    }
                })
            });

            if let Some(bvar) = block_var {
                if let Some(EdgeKind::Discarded {
                    expr_ty,
                    expr_span,
                    semi_span,
                }) = self.graph.discard_for(bvar)
                {
                    let resolved_discarded = self.subst.apply(expr_ty);
                    diag = diag.secondary(
                        *expr_span,
                        format!(
                            "this expression has type `{resolved_discarded}` — discarded by `;`"
                        ),
                    );
                    if resolved_discarded == *expected {
                        diag = diag.secondary(*semi_span, "value discarded here").suggest(
                            *semi_span,
                            "remove the `;` to return this value",
                            "",
                        );
                    } else {
                        diag = diag.secondary(*semi_span, "`;` makes this block return `()`");
                    }
                }
            }
        }

        // ── Inference trail ───────────────────────────────────────────────────

        for entry in trail {
            use crate::graph::TrailRole;

            if entry.step.span.is_detached() {
                continue;
            }

            let msg = match &entry.role {
                TrailRole::Introduction => {
                    let name = self.namer.name_for(entry.var);
                    format!("`{name}` introduced here with unknown type")
                }
                TrailRole::Narrowing { ty } => {
                    let name = self.namer.name_for(entry.var);
                    format!("`{name}` inferred as `{ty}` from this expression")
                }
                TrailRole::StructuralIntroduction { parent, position } => {
                    let parent_name = self.namer.name_for(*parent);
                    format!(
                        "`{parent_name}` constructed here — its {} is not yet known",
                        position.describe()
                    )
                }
                TrailRole::Relation => continue,
            };

            diag = diag.secondary(entry.step.span, msg);
        }

        diag
    }

    fn render_literal_mismatch(
        &mut self,
        kind: &LiteralKind,
        expected: &Ty,
        found: &Ty,
        literal_span: Span,
        trail: &[TrailEntry],
    ) -> Diagnostic {
        let forcing = trail.iter().rev().find(|entry| {
            matches!(entry.role, crate::graph::TrailRole::Narrowing { .. })
                && entry.step.span != literal_span
        });

        if let Some(entry) = forcing {
            let is_assignment = matches!(entry.step.reason, InferenceReason::Assignment { .. });
            let source_name = self
                .namer
                .has_source_name(entry.var)
                .then(|| self.namer.name_for(entry.var));
            let inferred_message = if let Some(name) = source_name.as_deref() {
                if is_assignment {
                    let source = if literal_came_from_assignment(literal_span, trail) {
                        "assignment"
                    } else {
                        "initializer"
                    };
                    format!("`{name}` was inferred as `{expected}` from this {source}")
                } else {
                    format!("`{name}` was inferred as `{expected}` here")
                }
            } else {
                format!("this {kind} literal was inferred as `{expected}` here")
            };
            let primary_message = if let Some(name) = source_name.as_deref() {
                if is_assignment {
                    format!(
                        "this assignment gives `{name}` a `{found}`, but `{expected}` was expected"
                    )
                } else {
                    format!("expected `{expected}`, found `{found}`")
                }
            } else if is_assignment {
                format!("this assignment gives `{found}`, but `{expected}` was expected")
            } else {
                format!("expected `{expected}`, found `{found}`")
            };

            return Diagnostic::error(format!(
                "mismatched types: expected `{expected}`, found `{found}`"
            ))
            .primary(entry.step.span, primary_message)
            .secondary(literal_span, inferred_message);
        }

        Diagnostic::error(format!("{kind} literal cannot be used as `{found}`"))
            .primary(literal_span, format!("this is a {kind} literal"))
    }

    fn render_bound_not_satisfied(
        &self,
        ty: &Ty,
        interface: &InterfaceName,
        origin: &BoundOrigin,
        span: Span,
    ) -> Diagnostic {
        let mut diag = Diagnostic::error(format!("`{ty}` does not implement `{interface}`"))
            .primary(span, format!("`{ty}` does not implement `{interface}`"));

        match origin {
            BoundOrigin::FunctionBound {
                fn_name,
                param_name,
                bound_decl_span,
                call_span,
            } => {
                diag = diag
                    .secondary(
                        *bound_decl_span,
                        format!("`{param_name:?}: {interface}` required by `{fn_name:?}` here"),
                    )
                    .secondary(*call_span, "called here");
            }
            BoundOrigin::DynCoercion { expected_span, .. } => {
                diag = diag.secondary(*expected_span, format!("`dyn {interface}` expected here"));
            }
            BoundOrigin::MethodCall {
                method_name,
                call_span,
                ..
            } => {
                diag = diag.secondary(
                    *call_span,
                    format!("method `{method_name:?}` requires `{interface}`"),
                );
            }
        }

        diag
    }

    fn render_missing_bound(
        &self,
        type_param: &TypeVar,
        required: &InterfaceName,
        origin: &BoundOrigin,
        span: Span,
        declared_bounds: &[BoundEntry],
    ) -> Diagnostic {
        let name = self.namer.name_for(*type_param);

        let mut diag = Diagnostic::error(format!("`{name}` does not implement `{required}`"))
            .primary(span, format!("requires `{name}: {required}`"));

        match origin {
            BoundOrigin::MethodCall { method_name, .. } => {
                diag = diag.secondary(
                    span,
                    format!("method `{method_name:?}` requires `{required}`"),
                );
            }
            _ => {}
        }

        if declared_bounds.is_empty() {
            diag = diag
                .note(format!("`{name}` has no bounds in the function signature"))
                .suggest(
                    declared_bounds
                        .first()
                        .map(|b| b.declared_at)
                        .unwrap_or(span),
                    format!("add the required bound: `{name}: {required}`"),
                    format!("{name}: {required}"),
                );
        } else {
            let bounds_str = declared_bounds
                .iter()
                .map(|b| b.interface.to_string())
                .collect::<Vec<_>>()
                .join(" + ");
            diag = diag.note(format!(
                "`{name}` only has these bounds: `{bounds_str}` — `{required}` is not among them"
            ));
        }

        diag
    }
}

fn literal_came_from_assignment(literal_span: Span, trail: &[TrailEntry]) -> bool {
    trail.iter().any(|entry| {
        entry.step.span == literal_span
            && matches!(entry.role, crate::graph::TrailRole::Relation)
            && matches!(entry.step.reason, InferenceReason::Assignment { .. })
    })
}
