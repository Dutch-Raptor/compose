use crate::{
    intern::InterfaceName,
    ty::{LiteralKind, Ty},
};
use compose_syntax::Span;

// ── Constraint Origins ────────────────────────────────────────────────────────
//
// Every constraint carries an origin so that when it participates in an error,
// the diagnostic can point at the right source location and explain *why* the
// constraint was created.

#[derive(Debug, Clone)]
pub enum ConstraintOrigin {
    /// `let x: T = expr` — binding type vs annotation.
    LetBinding {
        binding_span: Span,
        value_span: Span,
    },
    /// `x = expr` — assigned value must match the existing binding type.
    Assignment { target_span: Span, value_span: Span },
    /// Body type vs declared return type.
    ReturnType {
        fn_span: Span,
        return_expr_span: Span,
    },
    /// An argument passed to a function call.
    FunctionArg {
        call_span: Span,
        arg_span: Span,
        param_index: usize,
    },
    /// Two branches of an if/match must unify.
    BranchArm {
        first_arm_span: Span,
        this_arm_span: Span,
    },
    /// A method call constrained a generic parameter.
    MethodCall {
        receiver_span: Span,
        method_span: Span,
        arg_span: Span,
    },
    /// Explicit type annotation: `let x: i32 = ...`
    Annotation { annotated_span: Span },
    /// A binary operator requires specific operand types.
    BinaryOp {
        op_span: Span,
        left_span: Span,
        right_span: Span,
    },
}

impl ConstraintOrigin {
    /// The primary span for this origin — used as a fallback.
    pub fn primary_span(&self) -> Span {
        match self {
            ConstraintOrigin::LetBinding { binding_span, .. } => *binding_span,
            ConstraintOrigin::Assignment { value_span, .. } => *value_span,
            ConstraintOrigin::ReturnType {
                return_expr_span, ..
            } => *return_expr_span,
            ConstraintOrigin::FunctionArg { arg_span, .. } => *arg_span,
            ConstraintOrigin::BranchArm { this_arm_span, .. } => *this_arm_span,
            ConstraintOrigin::MethodCall { arg_span, .. } => *arg_span,
            ConstraintOrigin::Annotation { annotated_span } => *annotated_span,
            ConstraintOrigin::BinaryOp { op_span, .. } => *op_span,
        }
    }
}

// ── Bound Origins ─────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum BoundOrigin {
    /// A value was used where `dyn Interface` was expected.
    DynCoercion {
        expected_span: Span,
        value_span: Span,
    },
}

// ── Constraints ───────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum Constraint {
    /// Standard equality: `lhs ~ rhs`.
    Eq {
        lhs: Ty,
        rhs: Ty,
        origin: ConstraintOrigin,
        span: Span,
    },

    /// A literal's type must be a valid literal type for `kind`.
    /// If still unresolved after Eq solving, falls back to `default`.
    Literal {
        ty: Ty,
        kind: LiteralKind,
        default: Ty,
        span: Span,
    },

    /// The expression `expr_ty` was discarded by a semicolon, making
    /// `block_ty` (the enclosing block's result type) Unit.
    UnitCoercion {
        expr_ty: Ty,
        block_ty: Ty,
        expr_span: Span,
        semi_span: Span,
    },

    /// `ty` must implement `interface` (with optional generic arguments).
    Bound {
        ty: Ty,
        interface: InterfaceName,
        interface_args: Vec<Ty>,
        origin: BoundOrigin,
        span: Span,
    },
}

impl Constraint {
    pub fn span(&self) -> Span {
        match self {
            Constraint::Eq { span, .. } => *span,
            Constraint::Literal { span, .. } => *span,
            Constraint::UnitCoercion { semi_span, .. } => *semi_span,
            Constraint::Bound { span, .. } => *span,
        }
    }
}

// ── Partition ─────────────────────────────────────────────────────────────────

/// Split a flat list of constraints into buckets for each solve phase.
pub struct PartitionedConstraints {
    pub eq: Vec<Constraint>,
    pub literal: Vec<Constraint>,
    pub unit_coercion: Vec<Constraint>,
    pub bound: Vec<Constraint>,
}

pub fn partition(constraints: Vec<Constraint>) -> PartitionedConstraints {
    let mut eq = Vec::new();
    let mut literal = Vec::new();
    let mut unit_coercion = Vec::new();
    let mut bound = Vec::new();

    for c in constraints {
        match &c {
            Constraint::Eq { .. } => eq.push(c),
            Constraint::Literal { .. } => literal.push(c),
            Constraint::UnitCoercion { .. } => unit_coercion.push(c),
            Constraint::Bound { .. } => bound.push(c),
        }
    }

    PartitionedConstraints {
        eq,
        literal,
        unit_coercion,
        bound,
    }
}

/// Public name for constraint provenance consumed by LSP and diagnostic callers.
pub type ConstraintSource = ConstraintOrigin;
