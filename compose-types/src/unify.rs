use crate::{
    bounds::{InterfaceTable, TypeParamEnv},
    constraint::{partition, BoundOrigin, Constraint, ConstraintOrigin},
    error::TypeError,
    graph::{InferenceGraph, InferenceReason, InferenceStep},
    subst::Substitution,
    ty::{Ty, TypeVar},
};
use compose_resolve::SymbolId;
use compose_syntax::Span;

// ── Unifier ───────────────────────────────────────────────────────────────────

pub struct Unifier<'env> {
    pub subst: Substitution,
    pub errors: Vec<TypeError>,
    pub graph: InferenceGraph,
    /// Active only while checking a generic function body.
    /// None at call sites.
    pub type_param_env: Option<&'env TypeParamEnv>,
    pub interface_table: &'env InterfaceTable,
}

impl<'env> Unifier<'env> {
    pub fn new(interface_table: &'env InterfaceTable) -> Self {
        Unifier {
            subst: Substitution::default(),
            errors: Vec::new(),
            graph: InferenceGraph::default(),
            type_param_env: None,
            interface_table,
        }
    }

    pub fn with_type_param_env(mut self, env: &'env TypeParamEnv) -> Self {
        self.type_param_env = Some(env);
        self
    }

    // ── Main entry point ──────────────────────────────────────────────────────

    /// Solve all constraints in the correct phase order.
    pub fn solve(&mut self, constraints: Vec<Constraint>) {
        let parts = partition(constraints);

        // Phase 1: Equality — propagate all structural information.
        for c in parts.eq {
            self.solve_eq(c);
        }

        // Phase 2: UnitCoercion — bind discarded vars to Unit.
        // Must run before literal defaults so a discarded int literal
        // correctly becomes Unit rather than defaulting to int first.
        for c in parts.unit_coercion {
            self.solve_unit_coercion(c);
        }

        // Phase 3: Literal defaults — pin unresolved literal vars.
        for c in parts.literal {
            self.solve_literal(c);
        }

        // Phase 4: Bounds — verify interface satisfaction.
        for c in parts.bound {
            self.solve_bound(c);
        }
    }

    // ── Phase 1: Equality ─────────────────────────────────────────────────────

    fn solve_eq(&mut self, constraint: Constraint) {
        let Constraint::Eq {
            lhs,
            rhs,
            origin,
            span,
        } = constraint
        else {
            unreachable!()
        };

        // Apply current substitution before unifying.
        let lhs = self.subst.apply(&lhs);
        let rhs = self.subst.apply(&rhs);

        if let Err(e) = self.unify(lhs, rhs, &origin, span) {
            self.errors.push(e);
            // Continue — downstream errors will hit Ty::Error and be absorbed.
        }
    }

    fn unify(
        &mut self,
        lhs: Ty,
        rhs: Ty,
        origin: &ConstraintOrigin,
        span: Span,
    ) -> Result<(), TypeError> {
        let lhs = self.subst.apply(&lhs);
        let rhs = self.subst.apply(&rhs);

        match (lhs, rhs) {
            // ── Trivial cases ─────────────────────────────────────────────────
            (Ty::Int, Ty::Int)
            | (Ty::Bool, Ty::Bool)
            | (Ty::Float, Ty::Float)
            | (Ty::Str, Ty::Str)
            | (Ty::Unit, Ty::Unit) => Ok(()),

            // Error absorbs everything — no cascading errors.
            (Ty::Error, _) | (_, Ty::Error) => Ok(()),

            // ── Type variable cases ───────────────────────────────────────────

            // Two unresolved vars: merge in union-find, transfer graph histories.
            (Ty::Var(v1), Ty::Var(v2)) if v1 == v2 => Ok(()),
            (Ty::Var(v1), Ty::Var(v2)) => {
                let c1 = self.subst.canonical(v1);
                let c2 = self.subst.canonical(v2);
                if c1 == c2 {
                    return Ok(());
                }

                match (
                    self.subst.binding(c1).cloned(),
                    self.subst.binding(c2).cloned(),
                ) {
                    (Some(lhs), Some(rhs)) if lhs != rhs => {
                        return Err(self.make_mismatch(lhs, rhs, origin, span));
                    }
                    _ => {}
                }

                let step = inference_step_from_origin(origin, &Ty::Var(v2), span);
                self.subst.union_vars(c1, c2, step, &mut self.graph);
                Ok(())
            }

            // Var on the left: bind var to concrete type.
            (Ty::Var(v), ty) => self.bind_var(v, ty, origin, span),

            // Var on the right: bind var to concrete type.
            (ty, Ty::Var(v)) => self.bind_var(v, ty, origin, span),

            // ── Structural cases ──────────────────────────────────────────────

            // App: names must match, then recurse on each arg.
            (Ty::App(n1, args1), Ty::App(n2, args2)) if n1 == n2 => {
                if args1.len() != args2.len() {
                    return Err(self.make_mismatch(
                        Ty::App(n1, args1),
                        Ty::App(n2, args2),
                        origin,
                        span,
                    ));
                }
                for (a, b) in args1.into_iter().zip(args2) {
                    self.unify(a, b, origin, span)?;
                }
                Ok(())
            }

            // Fn: arity must match, then recurse on each param and return.
            (Ty::Fn(p1, r1), Ty::Fn(p2, r2)) if p1.len() == p2.len() => {
                for (a, b) in p1.into_iter().zip(p2) {
                    self.unify(a, b, origin, span)?;
                }
                self.unify(*r1, *r2, origin, span)
            }
            (
                Ty::VariadicFn {
                    params: p1,
                    variadic: v1,
                    ret: r1,
                },
                Ty::VariadicFn {
                    params: p2,
                    variadic: v2,
                    ret: r2,
                },
            ) if p1.len() == p2.len() => {
                for (a, b) in p1.into_iter().zip(p2) {
                    self.unify(a, b, origin, span)?;
                }
                self.unify(*v1, *v2, origin, span)?;
                self.unify(*r1, *r2, origin, span)
            }

            // Concrete type flowing into a dyn position:
            // emit a Bound constraint to verify the impl, let phase 4 check it.
            (concrete, Ty::Dyn(iface, iface_args)) => {
                // Record the coercion in the graph for diagnostics.
                for var in concrete.free_vars() {
                    let _canonical = self.subst.canonical(var);
                    // We'll record it at the bound-solving phase.
                }
                // Synthesise a Bound constraint for phase 4.
                // We push it directly rather than emitting via the collector
                // since we're already inside the solver.
                let bound = Constraint::Bound {
                    ty: concrete,
                    interface: iface,
                    interface_args: iface_args,
                    origin: BoundOrigin::DynCoercion {
                        expected_span: span,
                        value_span: span,
                    },
                    span,
                };
                self.solve_bound(bound);
                Ok(())
            }

            // ── Mismatch ──────────────────────────────────────────────────────
            (lhs, rhs) => Err(self.make_mismatch(lhs, rhs, origin, span)),
        }
    }

    fn bind_var(
        &mut self,
        var: TypeVar,
        ty: Ty,
        origin: &ConstraintOrigin,
        span: Span,
    ) -> Result<(), TypeError> {
        let canonical = self.subst.canonical(var);
        if let Some(existing) = self.subst.binding(canonical).cloned() {
            if existing != ty {
                return Err(self.make_mismatch(existing, ty, origin, span));
            }
            return Ok(());
        }

        let step = inference_step_from_origin(origin, &ty, span);
        self.subst
            .bind_with_evidence(canonical, ty, step, &mut self.graph)
            .map_err(|e| TypeError::InfiniteType {
                var: e.var,
                ty: e.ty,
                span,
            })
    }

    fn make_mismatch(
        &mut self,
        expected: Ty,
        found: Ty,
        origin: &ConstraintOrigin,
        span: Span,
    ) -> TypeError {
        // Collect the inference trail for all vars mentioned in `found`.
        let trail = self.graph.collect_trails_for_ty(&found);
        TypeError::Mismatch {
            expected,
            found,
            origin: origin.clone(),
            span,
            inference_trail: trail,
        }
    }

    // ── Phase 2: UnitCoercion ─────────────────────────────────────────────────

    fn solve_unit_coercion(&mut self, constraint: Constraint) {
        let Constraint::UnitCoercion {
            expr_ty,
            block_ty,
            expr_span,
            semi_span,
        } = constraint
        else {
            unreachable!()
        };

        let resolved_expr = self.subst.apply(&expr_ty);
        let resolved_block = self.subst.apply(&block_ty);

        if let Ty::Var(block_var) = resolved_block {
            let canonical = self.subst.canonical(block_var);
            let step = InferenceStep {
                ty: Ty::Unit,
                span: semi_span,
                reason: InferenceReason::UnitCoercion { expr_span },
            };
            if let Err(e) =
                self.subst
                    .bind_with_evidence(canonical, Ty::Unit, step, &mut self.graph)
            {
                self.errors.push(TypeError::InfiniteType {
                    var: e.var,
                    ty: e.ty,
                    span: semi_span,
                });
            }
        }

        if !matches!(resolved_expr, Ty::Unit | Ty::Error) {
            if let Ty::Var(block_var) = self.subst.apply(&block_ty) {
                let canonical = self.subst.canonical(block_var);
                self.graph
                    .record_discard(canonical, resolved_expr, expr_span, semi_span);
            }
        }
    }

    // ── Phase 3: Literal defaults ─────────────────────────────────────────────

    fn solve_literal(&mut self, constraint: Constraint) {
        let Constraint::Literal {
            ty,
            kind,
            default,
            span,
        } = constraint
        else {
            unreachable!()
        };

        let resolved = self.subst.apply(&ty);

        match resolved {
            // Already pinned by an Eq constraint — just validate.
            Ty::Int | Ty::Float | Ty::Str => {} // ok
            Ty::Bool
            | Ty::Unit
            | Ty::App(..)
            | Ty::Fn(..)
            | Ty::VariadicFn { .. }
            | Ty::Dyn(..) => {
                let inference_trail = match ty {
                    Ty::Var(var) => {
                        let canonical = self.subst.canonical(var);
                        self.graph.collect_trail(canonical)
                    }
                    _ => self.graph.collect_trails_for_ty(&ty),
                };
                self.errors.push(TypeError::LiteralTypeMismatch {
                    kind,
                    expected: default,
                    found: resolved,
                    span,
                    inference_trail,
                });
            }

            // Still unresolved — apply the default.
            Ty::Var(v) => {
                let canonical = self.subst.canonical(v);
                let step = InferenceStep {
                    ty: default.clone(),
                    span,
                    reason: InferenceReason::LiteralDefault { kind },
                };
                if let Err(e) =
                    self.subst
                        .bind_with_evidence(canonical, default, step, &mut self.graph)
                {
                    self.errors.push(TypeError::InfiniteType {
                        var: e.var,
                        ty: e.ty,
                        span,
                    });
                }
            }

            Ty::Error => {}
        }
    }

    // ── Phase 4: Bounds ───────────────────────────────────────────────────────

    fn solve_bound(&mut self, constraint: Constraint) {
        let Constraint::Bound {
            ty,
            interface,
            interface_args,
            origin,
            span,
        } = constraint
        else {
            unreachable!()
        };

        let resolved = self.subst.apply(&ty);

        match resolved {
            // Error — absorb silently.
            Ty::Error => {}

            // Unresolved type variable.
            Ty::Var(v) => {
                let canonical = self.subst.canonical(v);
                match self.type_param_env {
                    // Inside a generic function body — check declared bounds.
                    Some(env) => {
                        if !env.satisfies(&canonical, &interface) {
                            let declared = env.bounds_for(&canonical).to_vec();
                            self.errors.push(TypeError::MissingBoundInSignature {
                                type_param: canonical,
                                required: interface,
                                origin,
                                span,
                                declared_bounds: declared,
                            });
                        }
                        // else: bound satisfied by declaration — ok.
                    }
                    // Not inside a generic function — truly ambiguous.
                    None => {
                        self.errors.push(TypeError::AmbiguousType {
                            var: canonical,
                            span,
                        });
                    }
                }
            }

            // Concrete type — check the interface table.
            concrete => {
                if !self.interface_table.implements(&concrete, &interface) {
                    self.errors.push(TypeError::BoundNotSatisfied {
                        ty: concrete,
                        interface,
                        interface_args,
                        origin,
                        span,
                    });
                }
            }
        }
    }
}

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Build an InferenceStep from a constraint origin and the type being bound.
fn inference_step_from_origin(origin: &ConstraintOrigin, ty: &Ty, span: Span) -> InferenceStep {
    let (step_span, reason) = match origin {
        ConstraintOrigin::MethodCall {
            arg_span,
            method_span: _,
            ..
        } => (
            *arg_span,
            InferenceReason::MethodArgument {
                method: synthetic_symbol_id(),
                arg_index: 0,
            },
        ),
        ConstraintOrigin::LetBinding { value_span, .. } => (
            *value_span,
            InferenceReason::Construction {
                method: synthetic_symbol_id(),
            },
        ),
        ConstraintOrigin::Assignment {
            target_span,
            value_span,
        } => (
            *value_span,
            InferenceReason::Assignment {
                target_span: *target_span,
            },
        ),
        ConstraintOrigin::Annotation { annotated_span } => {
            (*annotated_span, InferenceReason::Annotation)
        }
        ConstraintOrigin::ReturnType {
            return_expr_span, ..
        } => (
            *return_expr_span,
            InferenceReason::CallReturn {
                fn_name: synthetic_symbol_id(),
            },
        ),
        ConstraintOrigin::FunctionArg { arg_span, .. } => (
            *arg_span,
            InferenceReason::MethodArgument {
                method: synthetic_symbol_id(),
                arg_index: 0,
            },
        ),
        ConstraintOrigin::BranchArm {
            this_arm_span,
            first_arm_span,
        } => (
            *this_arm_span,
            InferenceReason::BranchUnification {
                other_branch: *first_arm_span,
            },
        ),
        ConstraintOrigin::BinaryOp { op_span, .. } => {
            (*op_span, InferenceReason::BinaryOp { op_span: *op_span })
        }
        _ => (span, InferenceReason::Annotation),
    };

    InferenceStep {
        ty: ty.clone(),
        span: step_span,
        reason,
    }
}

fn synthetic_symbol_id() -> SymbolId {
    SymbolId::new(u64::MAX).expect("synthetic symbol id is non-zero")
}
