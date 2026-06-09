use std::collections::{HashMap, HashSet};

use compose_library::diag::{SourceDiagnostic, eco_format};
use compose_library::{Value, World};
use compose_resolve::symbol::{SymbolKind, SymbolOrigin, SymbolTable};
use compose_resolve::{ExprId, ExprIdTable, ResolutionResult, SymbolId};
use compose_syntax::ast::ty::Type;
use compose_syntax::ast::{
    self, Arg, AssignOp, AstNode, BinOp, Expr, FnItem, Ident, ParamKind, Pattern, Statement, UnOp,
};
use compose_syntax::{Label, Span, SyntaxKind, SyntaxNode};
use ecow::EcoString;
use fxhash::FxHashMap;

use crate::constraint::{BoundOrigin, Constraint, ConstraintOrigin};
use crate::env::TyEnv;
use crate::error::{DiagnosticRenderer, TypeError};
use crate::graph::InferenceGraph;
use crate::intern::TypeName;
use crate::namer::VarNamer;
use crate::subst::Substitution;
use crate::ty::{LiteralKind, Ty, TyVarGen, TypeVar};
use crate::unify::Unifier;

/// Type data attached to an expression through side tables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInfo {
    pub original: Ty,
    pub final_ty: Ty,
}

/// Full output of the static type checking pass.
#[derive(Debug)]
pub struct TypeCheckResult {
    pub expr_types: FxHashMap<ExprId, TypeInfo>,
    pub constraint_sources: FxHashMap<ExprId, Vec<ConstraintOrigin>>,
    pub diagnostics: Vec<SourceDiagnostic>,
    pub constraints: Vec<Constraint>,
    pub graph: InferenceGraph,
    pub substitution: Substitution,
}

/// Entry point used by the CLI and future LSP integrations.
pub struct InferenceEngine<'a> {
    world: &'a dyn World,
    expr_ids: &'a ExprIdTable,
    symbol_table: &'a SymbolTable,
    expr_to_symbol: &'a FxHashMap<ExprId, SymbolId>,
}

impl<'a> InferenceEngine<'a> {
    pub fn new(
        world: &'a dyn World,
        expr_ids: &'a ExprIdTable,
        symbol_table: &'a SymbolTable,
        expr_to_symbol: &'a FxHashMap<ExprId, SymbolId>,
    ) -> Self {
        Self {
            world,
            expr_ids,
            symbol_table,
            expr_to_symbol,
        }
    }

    pub fn from_resolution(
        world: &'a dyn World,
        expr_ids: &'a ExprIdTable,
        resolution: &'a ResolutionResult,
    ) -> Self {
        Self::new(
            world,
            expr_ids,
            &resolution.symbol_table,
            &resolution.expr_to_symbol,
        )
    }

    /// Infer and check the world entry point.
    pub fn infer(&mut self) -> TypeCheckResult {
        let source = match self.world.source(self.world.entry_point()) {
            Ok(source) => source,
            Err(err) => {
                return TypeCheckResult {
                    expr_types: FxHashMap::default(),
                    constraint_sources: FxHashMap::default(),
                    diagnostics: vec![SourceDiagnostic::error(
                        Span::detached(),
                        eco_format!("{err}"),
                    )],
                    constraints: Vec::new(),
                    graph: InferenceGraph::default(),
                    substitution: Substitution::default(),
                };
            }
        };

        let mut checker = TypeChecker::new(
            self.world,
            self.expr_ids,
            self.symbol_table,
            self.expr_to_symbol,
        );
        checker.check_root(source.root_node())
    }
}

/// Convenience helper for callers that already have a resolution result.
pub fn type_check(
    world: &dyn World,
    expr_ids: &ExprIdTable,
    resolution: &ResolutionResult,
) -> TypeCheckResult {
    InferenceEngine::from_resolution(world, expr_ids, resolution).infer()
}

#[derive(Debug, Clone)]
struct RawTypeInfo {
    original: Ty,
    final_ty: Ty,
}

#[derive(Debug, Clone)]
struct OperatorCheck {
    op: BinOp,
    left: Ty,
    right: Ty,
    result: Ty,
    span: Span,
    left_span: Span,
    right_span: Span,
}

#[derive(Debug, Clone)]
struct SemicolonCoercion {
    block_span: Span,
    expr_span: Span,
    semi_span: Span,
    expr_ty: Ty,
}

#[derive(Debug, Clone)]
struct UnitContext {
    span: Span,
    expected: Ty,
}

#[derive(Debug, Clone)]
struct ExpectedTy {
    ty: Ty,
    origin: ConstraintOrigin,
    span: Span,
}

#[derive(Debug, Clone)]
struct FnItemSignature {
    ty: Ty,
    params: Vec<Ty>,
    return_ty: Ty,
    declared_return_ty: Option<Ty>,
}

#[derive(Debug, Clone)]
struct ExprOutcome {
    ty: Ty,
    exits: bool,
}

#[derive(Debug, Clone)]
struct BlockOutcome {
    ty: Ty,
    exits: bool,
}

struct TypeChecker<'a> {
    world: &'a dyn World,
    expr_ids: &'a ExprIdTable,
    symbol_table: &'a SymbolTable,
    expr_to_symbol: &'a FxHashMap<ExprId, SymbolId>,
    var_gen: TyVarGen,
    constraints: Vec<Constraint>,
    raw_types: FxHashMap<ExprId, RawTypeInfo>,
    constraint_sources: FxHashMap<ExprId, Vec<ConstraintOrigin>>,
    graph: InferenceGraph,
    namer: VarNamer,
    type_names: HashMap<EcoString, TypeName>,
    current_return_ty: Option<Ty>,
    operator_checks: Vec<OperatorCheck>,
    semicolon_coercions: Vec<SemicolonCoercion>,
    unit_contexts: Vec<UnitContext>,
    fn_item_signatures: HashMap<SymbolId, FnItemSignature>,
}

impl<'a> TypeChecker<'a> {
    fn new(
        world: &'a dyn World,
        expr_ids: &'a ExprIdTable,
        symbol_table: &'a SymbolTable,
        expr_to_symbol: &'a FxHashMap<ExprId, SymbolId>,
    ) -> Self {
        let type_names = symbol_table
            .iter()
            .filter_map(|(id, symbol)| {
                (symbol.symbol_kind == SymbolKind::Type)
                    .then(|| (symbol.name.clone(), TypeName::source(*id)))
            })
            .collect();

        Self {
            world,
            expr_ids,
            symbol_table,
            expr_to_symbol,
            var_gen: TyVarGen::default(),
            constraints: Vec::new(),
            raw_types: FxHashMap::default(),
            constraint_sources: FxHashMap::default(),
            graph: InferenceGraph::default(),
            namer: VarNamer::default(),
            type_names,
            current_return_ty: None,
            operator_checks: Vec::new(),
            semicolon_coercions: Vec::new(),
            unit_contexts: Vec::new(),
            fn_item_signatures: HashMap::new(),
        }
    }

    fn check_root(&mut self, root: &SyntaxNode) -> TypeCheckResult {
        let mut env = TyEnv::default();

        let items = root
            .children()
            .filter_map(SyntaxNode::cast)
            .collect::<Vec<_>>();
        let statements = root
            .children()
            .filter_map(SyntaxNode::cast)
            .collect::<Vec<_>>();
        self.infer_code(&items, &statements, &mut env);

        let constraints = self.constraints.clone();
        let interface_table = self.world.library().type_info.interface_table().clone();
        let mut unifier = Unifier::new(&interface_table);
        unifier.graph = std::mem::take(&mut self.graph);
        unifier.solve(constraints.clone());

        let mut substitution = unifier.subst;
        let mut type_errors = unifier.errors;
        let poisoned_narrowing_spans = poisoned_narrowing_spans(&type_errors);
        let mut diagnostics = self.post_solve_diagnostics(
            &mut substitution,
            &unifier.graph,
            &poisoned_narrowing_spans,
        );
        let semicolon_diagnostics = self.semicolon_diagnostics(&mut substitution);
        let semicolon_spans = semicolon_diagnostics
            .iter()
            .map(|diag| diag.span)
            .collect::<Vec<_>>();
        diagnostics.extend(semicolon_diagnostics);
        diagnostics.extend(self.render_type_errors(
            &type_errors,
            &mut substitution,
            &semicolon_spans,
        ));

        let expr_types = self
            .raw_types
            .iter()
            .map(|(id, info)| {
                let original = substitution.apply(&info.original);
                let final_ty = substitution.apply(&info.final_ty);
                (*id, TypeInfo { original, final_ty })
            })
            .collect();

        type_errors.clear();

        TypeCheckResult {
            expr_types,
            constraint_sources: self.constraint_sources.clone(),
            diagnostics,
            constraints,
            graph: unifier.graph,
            substitution,
        }
    }

    fn render_type_errors(
        &self,
        errors: &[TypeError],
        substitution: &mut Substitution,
        skip_unit_mismatch_spans: &[Span],
    ) -> Vec<SourceDiagnostic> {
        let graph = InferenceGraph::default();
        let mut renderer = DiagnosticRenderer {
            graph: &graph,
            namer: &self.namer,
            subst: substitution,
        };

        let mut diagnostics = Vec::new();
        let mut seen = HashSet::new();

        for error in errors.iter().filter(|error| {
            !matches!(
                error,
                TypeError::Mismatch { found: Ty::Unit, span, .. }
                    if skip_unit_mismatch_spans.contains(span)
            )
        }) {
            let diagnostic = diagnostic_to_source(renderer.render(error));
            if seen.insert(diagnostic_dedupe_key(&diagnostic)) {
                diagnostics.push(diagnostic);
            }
        }

        diagnostics
    }

    fn semicolon_diagnostics(&self, substitution: &mut Substitution) -> Vec<SourceDiagnostic> {
        let mut diagnostics = Vec::new();

        for context in &self.unit_contexts {
            let expected = substitution.apply(&context.expected);
            if expected == Ty::Unit || expected.is_error() {
                continue;
            }

            let Some(coercion) = self
                .semicolon_coercions
                .iter()
                .find(|coercion| coercion.block_span == context.span)
            else {
                continue;
            };

            let discarded = substitution.apply(&coercion.expr_ty);
            if discarded != expected {
                continue;
            }

            diagnostics.push(
                SourceDiagnostic::error(
                    context.span,
                    eco_format!("mismatched types: expected `{expected}`, found `()`"),
                )
                .with_label_message("this block returns `()` because of the trailing semicolon")
                .with_label(Label::secondary(
                    coercion.expr_span,
                    eco_format!("this expression has type `{discarded}`"),
                ))
                .with_label(Label::secondary(coercion.semi_span, "value discarded here"))
                .with_hint("remove the trailing `;` to return this value"),
            );
        }

        diagnostics
    }

    fn post_solve_diagnostics(
        &self,
        substitution: &mut Substitution,
        graph: &InferenceGraph,
        poisoned_narrowing_spans: &HashSet<Span>,
    ) -> Vec<SourceDiagnostic> {
        let mut diagnostics = Vec::new();

        for check in &self.operator_checks {
            if self.operator_depends_on_poisoned_narrowing(
                check,
                substitution,
                graph,
                poisoned_narrowing_spans,
            ) {
                continue;
            }

            let left = substitution.apply(&check.left);
            let right = substitution.apply(&check.right);
            let result = substitution.apply(&check.result);

            match check.op {
                BinOp::Add => {
                    let ok = matches!(
                        (&left, &right, &result),
                        (Ty::Int, Ty::Int, Ty::Int) | (Ty::Str, Ty::Str, Ty::Str)
                    );
                    if !ok && !left.is_error() && !right.is_error() {
                        diagnostics.push(
                            SourceDiagnostic::error(
                                check.span,
                                eco_format!(
                                    "operator `{}` cannot be applied to `{left}` and `{right}`",
                                    check.op.descriptive_name()
                                ),
                            )
                            .with_label_message("invalid operands")
                            .with_label(Label::secondary(
                                check.left_span,
                                eco_format!("left operand has type `{left}`"),
                            ))
                            .with_label(Label::secondary(
                                check.right_span,
                                eco_format!("right operand has type `{right}`"),
                            )),
                        );
                    }
                }
                BinOp::Sub
                | BinOp::Mul
                | BinOp::Div
                | BinOp::Mod
                | BinOp::BitAnd
                | BinOp::BitOr
                | BinOp::BitXor
                | BinOp::BitShl
                | BinOp::BitShr => {
                    let ok = left == Ty::Int && right == Ty::Int && result == Ty::Int;
                    if !ok && !left.is_error() && !right.is_error() {
                        diagnostics.push(
                            SourceDiagnostic::error(
                                check.span,
                                eco_format!(
                                    "operator `{}` requires `int` operands",
                                    check.op.descriptive_name()
                                ),
                            )
                            .with_label_message(eco_format!(
                                "expected `int`, found `{left}` and `{right}`"
                            )),
                        );
                    }
                }
                _ => {}
            }
        }

        diagnostics
    }

    fn operator_depends_on_poisoned_narrowing(
        &self,
        check: &OperatorCheck,
        substitution: &mut Substitution,
        graph: &InferenceGraph,
        poisoned_narrowing_spans: &HashSet<Span>,
    ) -> bool {
        !poisoned_narrowing_spans.is_empty()
            && [
                check.left.clone(),
                check.right.clone(),
                check.result.clone(),
            ]
            .iter()
            .any(|ty| {
                ty_depends_on_poisoned_narrowing(ty, substitution, graph, poisoned_narrowing_spans)
            })
    }

    fn declare_fn_items(&mut self, items: &[FnItem<'_>], env: &mut TyEnv) {
        for item in items {
            self.declare_fn_item_signature(*item, env);
        }
    }

    fn infer_code(&mut self, items: &[FnItem<'_>], statements: &[Statement<'_>], env: &mut TyEnv) {
        self.declare_fn_items(items, env);

        for item in items {
            self.infer_fn_item(*item, env);
        }

        for statement in statements {
            self.infer_statement(*statement, env);
        }
    }

    fn infer_statement(&mut self, statement: Statement<'_>, env: &mut TyEnv) -> Ty {
        match statement {
            Statement::Expr(expr) => self.infer_expr(expr, env),
            Statement::Let(let_binding) => {
                self.infer_let(let_binding, env);
                Ty::Unit
            }
            Statement::Assign(assign) => {
                let lhs = self.infer_expr(assign.lhs(), env);
                let rhs = self.infer_expr(assign.rhs(), env);
                self.emit_eq(
                    lhs.clone(),
                    rhs.clone(),
                    ConstraintOrigin::Assignment {
                        target_span: assign.lhs().span(),
                        value_span: assign.rhs().span(),
                    },
                    assign.rhs().span(),
                );

                if assign.op() != AssignOp::Assign {
                    self.operator_checks.push(OperatorCheck {
                        op: assign_op_as_bin_op(assign.op()),
                        left: lhs.clone(),
                        right: rhs,
                        result: lhs,
                        span: assign.span(),
                        left_span: assign.lhs().span(),
                        right_span: assign.rhs().span(),
                    });
                }

                Ty::Unit
            }
            Statement::Return(ret) => {
                let value_ty = ret
                    .value()
                    .map(|expr| self.infer_expr(expr, env))
                    .unwrap_or(Ty::Unit);
                if let Some(expected) = self.current_return_ty.clone() {
                    self.emit_eq(
                        expected,
                        value_ty,
                        ConstraintOrigin::ReturnType {
                            fn_span: ret.span(),
                            return_expr_span: ret.value().map_or(ret.span(), |v| v.span()),
                        },
                        ret.span(),
                    );
                }
                Ty::Unit
            }
            Statement::Break(break_) => {
                if let Some(value) = break_.value() {
                    self.infer_expr(value, env);
                }
                Ty::Unit
            }
            Statement::Continue(_) | Statement::ModuleImport(_) => Ty::Unit,
        }
    }

    fn infer_let(&mut self, let_binding: ast::LetBinding<'_>, env: &mut TyEnv) {
        let initial_value = let_binding.initial_value();
        let binding_ty = if let Some(annotation) = let_binding.type_annotation() {
            let annotated = self.type_from_annotation(annotation);
            if let Some(expr) = initial_value {
                self.infer_expr_with_expected(
                    expr,
                    env,
                    Some(ExpectedTy {
                        ty: annotated.clone(),
                        origin: ConstraintOrigin::Annotation {
                            annotated_span: annotation.span(),
                        },
                        span: expr.span(),
                    }),
                );
            }
            annotated
        } else {
            initial_value
                .map(|expr| self.infer_expr(expr, env))
                .unwrap_or_else(|| self.var_gen.fresh_ty())
        };

        self.bind_pattern(let_binding.pattern(), binding_ty, env);
    }

    fn infer_expr(&mut self, expr: Expr<'_>, env: &mut TyEnv) -> Ty {
        self.infer_expr_with_expected(expr, env, None)
    }

    fn infer_expr_with_expected(
        &mut self,
        expr: Expr<'_>,
        env: &mut TyEnv,
        expected: Option<ExpectedTy>,
    ) -> Ty {
        self.infer_expr_outcome_with_expected(expr, env, expected)
            .ty
    }

    fn infer_expr_outcome_with_expected(
        &mut self,
        expr: Expr<'_>,
        env: &mut TyEnv,
        expected: Option<ExpectedTy>,
    ) -> ExprOutcome {
        let outcome = self.infer_expr_inner(expr, env, expected.clone());

        if let Some(expected) = expected {
            if !outcome.exits {
                self.emit_eq(
                    expected.ty,
                    outcome.ty.clone(),
                    expected.origin,
                    expected.span,
                );
            }
        }

        self.record_expr(expr.span(), outcome.ty.clone(), outcome.ty.clone());
        outcome
    }

    fn infer_expr_inner(
        &mut self,
        expr: Expr<'_>,
        env: &mut TyEnv,
        expected: Option<ExpectedTy>,
    ) -> ExprOutcome {
        match expr {
            Expr::CodeBlock(block) => {
                let outcome = self.infer_block_outcome_with_expected(
                    block.to_untyped(),
                    block.span(),
                    env,
                    expected,
                );
                return ExprOutcome {
                    ty: outcome.ty,
                    exits: outcome.exits,
                };
            }
            Expr::Conditional(conditional) => {
                return self.infer_conditional(conditional, env, expected);
            }
            Expr::MatchExpression(match_expr) => {
                return self.infer_match(match_expr, env, expected);
            }
            _ => {}
        }

        let ty = match expr {
            Expr::Int(int) => {
                let var = self.fresh_named_var(int.span(), "integer literal");
                let ty = Ty::Var(var);
                self.constraints.push(Constraint::Literal {
                    ty: ty.clone(),
                    kind: LiteralKind::Integer,
                    default: Ty::Int,
                    span: int.span(),
                });
                ty
            }
            Expr::Bool(_) => Ty::Bool,
            Expr::Str(_) => Ty::Str,
            Expr::Unit(_) => Ty::Unit,
            Expr::Ident(ident) => self.infer_ident(ident, env),
            Expr::Unary(unary) => self.infer_unary(unary, env),
            Expr::Binary(binary) => self.infer_binary(binary, env),
            Expr::FuncCall(call) => self.infer_call(call, env),
            Expr::FieldAccess(access) => {
                self.infer_expr(access.target(), env);
                self.var_gen.fresh_ty()
            }
            Expr::PathAccess(path) => {
                self.infer_expr(path.target(), env);
                self.var_gen.fresh_ty()
            }
            Expr::Parenthesized(parenthesized) => {
                return self.infer_expr_outcome_with_expected(parenthesized.expr(), env, expected);
            }
            Expr::WhileLoop(while_loop) => {
                let mut cond_env = env.child();
                let cond_ty = self.infer_expr(while_loop.condition().expr(), &mut cond_env);
                self.emit_eq(
                    Ty::Bool,
                    cond_ty,
                    ConstraintOrigin::Annotation {
                        annotated_span: while_loop.condition().span(),
                    },
                    while_loop.condition().span(),
                );
                self.infer_block(
                    while_loop.body().to_untyped(),
                    while_loop.body().span(),
                    &mut env.child(),
                );
                Ty::Unit
            }
            Expr::ForLoop(for_loop) => {
                let item_ty = self.var_gen.fresh_ty();
                let iterable_ty = self.infer_expr(for_loop.iterable(), env);
                let iterator_ty = self.app_ty("Iterator", vec![item_ty.clone()]);
                self.emit_eq(
                    iterator_ty,
                    iterable_ty,
                    ConstraintOrigin::Annotation {
                        annotated_span: for_loop.iterable().span(),
                    },
                    for_loop.iterable().span(),
                );

                let mut body_env = env.child();
                self.bind_pattern(for_loop.binding(), item_ty, &mut body_env);
                self.infer_block(
                    for_loop.body().to_untyped(),
                    for_loop.body().span(),
                    &mut body_env,
                );
                Ty::Unit
            }
            Expr::Array(array) => self.infer_array(array, env),
            Expr::Range(range) => {
                for endpoint in range.start().into_iter().chain(range.end()) {
                    let endpoint_ty = self.infer_expr(endpoint, env);
                    self.emit_eq(
                        Ty::Int,
                        endpoint_ty,
                        ConstraintOrigin::Annotation {
                            annotated_span: endpoint.span(),
                        },
                        endpoint.span(),
                    );
                }
                self.app_ty("Range", vec![Ty::Int])
            }
            Expr::Map(map) => self.infer_map(map, env),
            Expr::Lambda(lambda) => self.infer_lambda(lambda, env),
            Expr::IndexAccess(index) => {
                let elem_ty = self.var_gen.fresh_ty();
                let target_ty = self.infer_expr(index.target(), env);
                let index_ty = self.infer_expr(index.index(), env);
                let array_ty = self.app_ty("Array", vec![elem_ty.clone()]);
                self.emit_eq(
                    array_ty,
                    target_ty,
                    ConstraintOrigin::FunctionArg {
                        call_span: index.span(),
                        arg_span: index.target().span(),
                        param_index: 0,
                    },
                    index.target().span(),
                );
                self.emit_eq(
                    Ty::Int,
                    index_ty,
                    ConstraintOrigin::FunctionArg {
                        call_span: index.span(),
                        arg_span: index.index().span(),
                        param_index: 1,
                    },
                    index.index().span(),
                );
                elem_ty
            }
            Expr::IsExpression(is_expr) => {
                let subject_ty = self.infer_expr(is_expr.expr(), env);
                self.bind_pattern(is_expr.pattern(), subject_ty, env);
                Ty::Bool
            }
            Expr::CodeBlock(_) | Expr::Conditional(_) | Expr::MatchExpression(_) => unreachable!(),
        };

        ExprOutcome { ty, exits: false }
    }

    fn infer_ident(&mut self, ident: Ident<'_>, env: &TyEnv) -> Ty {
        let Some(symbol_id) = self.symbol_for_span(ident.span()) else {
            return Ty::Error;
        };

        if let Some(ty) = env.lookup(symbol_id) {
            return ty.clone();
        }

        let Some(symbol) = self.symbol_table.get(symbol_id) else {
            return Ty::Error;
        };

        match symbol.symbol_origin {
            SymbolOrigin::Capture { source, .. } => env
                .lookup(source)
                .cloned()
                .unwrap_or_else(|| self.var_gen.fresh_ty()),
            SymbolOrigin::Implicit => self.implicit_symbol_ty(symbol_id),
            _ => self.var_gen.fresh_ty(),
        }
    }

    fn infer_unary(&mut self, unary: ast::Unary<'_>, env: &mut TyEnv) -> Ty {
        let operand = self.infer_expr(unary.expr(), env);
        match unary.op() {
            UnOp::Plus | UnOp::Minus | UnOp::Tilde => {
                self.emit_eq(
                    Ty::Int,
                    operand,
                    ConstraintOrigin::BinaryOp {
                        op_span: unary.span(),
                        left_span: unary.expr().span(),
                        right_span: unary.expr().span(),
                    },
                    unary.span(),
                );
                Ty::Int
            }
            UnOp::Bang => {
                self.emit_eq(
                    Ty::Bool,
                    operand,
                    ConstraintOrigin::BinaryOp {
                        op_span: unary.span(),
                        left_span: unary.expr().span(),
                        right_span: unary.expr().span(),
                    },
                    unary.span(),
                );
                Ty::Bool
            }
            UnOp::Star => self.var_gen.fresh_ty(),
        }
    }

    fn infer_binary(&mut self, binary: ast::Binary<'_>, env: &mut TyEnv) -> Ty {
        let left_ty = self.infer_expr(binary.lhs(), env);
        let right_ty = self.infer_expr(binary.rhs(), env);
        let op = binary.op();

        match op {
            BinOp::And | BinOp::Or => {
                self.emit_eq(
                    Ty::Bool,
                    left_ty,
                    ConstraintOrigin::BinaryOp {
                        op_span: binary.span(),
                        left_span: binary.lhs().span(),
                        right_span: binary.rhs().span(),
                    },
                    binary.lhs().span(),
                );
                self.emit_eq(
                    Ty::Bool,
                    right_ty,
                    ConstraintOrigin::BinaryOp {
                        op_span: binary.span(),
                        left_span: binary.lhs().span(),
                        right_span: binary.rhs().span(),
                    },
                    binary.rhs().span(),
                );
                Ty::Bool
            }
            BinOp::Eq | BinOp::Neq => {
                self.emit_eq(
                    left_ty,
                    right_ty,
                    ConstraintOrigin::BinaryOp {
                        op_span: binary.span(),
                        left_span: binary.lhs().span(),
                        right_span: binary.rhs().span(),
                    },
                    binary.span(),
                );
                Ty::Bool
            }
            BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte => {
                self.emit_eq(
                    Ty::Int,
                    left_ty,
                    ConstraintOrigin::BinaryOp {
                        op_span: binary.span(),
                        left_span: binary.lhs().span(),
                        right_span: binary.rhs().span(),
                    },
                    binary.lhs().span(),
                );
                self.emit_eq(
                    Ty::Int,
                    right_ty,
                    ConstraintOrigin::BinaryOp {
                        op_span: binary.span(),
                        left_span: binary.lhs().span(),
                        right_span: binary.rhs().span(),
                    },
                    binary.rhs().span(),
                );
                Ty::Bool
            }
            _ => {
                self.emit_eq(
                    left_ty.clone(),
                    right_ty.clone(),
                    ConstraintOrigin::BinaryOp {
                        op_span: binary.span(),
                        left_span: binary.lhs().span(),
                        right_span: binary.rhs().span(),
                    },
                    binary.span(),
                );
                let result_ty = left_ty.clone();
                self.operator_checks.push(OperatorCheck {
                    op,
                    left: left_ty,
                    right: right_ty,
                    result: result_ty.clone(),
                    span: binary.span(),
                    left_span: binary.lhs().span(),
                    right_span: binary.rhs().span(),
                });
                result_ty
            }
        }
    }

    fn infer_call(&mut self, call: ast::FuncCall<'_>, env: &mut TyEnv) -> Ty {
        if let Expr::FieldAccess(access) = call.callee() {
            return self.infer_method_call(access, call.args(), call.span(), env);
        }

        let callee_ty = self.infer_expr(call.callee(), env);
        let args = call.args().items().collect::<Vec<_>>();
        let arg_tys = args
            .iter()
            .copied()
            .map(|arg| self.infer_arg(arg, env))
            .collect::<Vec<_>>();
        let ret_ty = self.var_gen.fresh_ty();

        if let Ty::VariadicFn {
            params,
            variadic,
            ret,
        } = callee_ty.clone()
        {
            for (index, (arg, arg_ty)) in args.iter().zip(arg_tys.iter()).enumerate() {
                let expected = params
                    .get(index)
                    .cloned()
                    .unwrap_or_else(|| (*variadic).clone());
                self.emit_call_arg_constraint(
                    arg_ty.clone(),
                    expected,
                    call.span(),
                    arg.span(),
                    index,
                );
            }
            return *ret;
        }

        self.emit_eq(
            Ty::Fn(arg_tys, Box::new(ret_ty.clone())),
            callee_ty,
            ConstraintOrigin::FunctionArg {
                call_span: call.span(),
                arg_span: call.callee().span(),
                param_index: 0,
            },
            call.span(),
        );

        ret_ty
    }

    fn emit_call_arg_constraint(
        &mut self,
        arg_ty: Ty,
        expected_ty: Ty,
        call_span: Span,
        arg_span: Span,
        param_index: usize,
    ) {
        if let Ty::Dyn(interface, interface_args) = expected_ty {
            self.constraints.push(Constraint::Bound {
                ty: arg_ty,
                interface,
                interface_args,
                origin: BoundOrigin::DynCoercion {
                    expected_span: call_span,
                    value_span: arg_span,
                },
                span: arg_span,
            });
        } else {
            self.emit_eq(
                arg_ty,
                expected_ty,
                ConstraintOrigin::FunctionArg {
                    call_span,
                    arg_span,
                    param_index,
                },
                arg_span,
            );
        }
    }

    fn infer_method_call(
        &mut self,
        access: ast::FieldAccess<'_>,
        args: ast::Args<'_>,
        call_span: Span,
        env: &mut TyEnv,
    ) -> Ty {
        let receiver_ty = self.infer_expr(access.target(), env);
        let args = args.items().collect::<Vec<_>>();
        let arg_tys = args
            .iter()
            .copied()
            .map(|arg| self.infer_arg(arg, env))
            .collect::<Vec<_>>();
        let method_name = access.field().as_str();

        match method_name {
            "push" if arg_tys.len() == 1 => {
                let arg_span = args[0].span();
                let elem_ty = self.var_gen.fresh_ty();
                let array_ty = self.app_ty("Array", vec![elem_ty.clone()]);
                self.emit_eq(
                    array_ty,
                    receiver_ty,
                    ConstraintOrigin::FunctionArg {
                        call_span,
                        arg_span: access.target().span(),
                        param_index: 0,
                    },
                    access.target().span(),
                );
                self.emit_eq(
                    elem_ty,
                    arg_tys[0].clone(),
                    ConstraintOrigin::MethodCall {
                        receiver_span: access.target().span(),
                        method_span: access.field().span(),
                        arg_span,
                    },
                    arg_span,
                );
                Ty::Unit
            }
            "len" => Ty::Int,
            "to_string" => Ty::Str,
            "clone" => receiver_ty,
            _ => self.var_gen.fresh_ty(),
        }
    }

    fn infer_arg(&mut self, arg: Arg<'_>, env: &mut TyEnv) -> Ty {
        match arg {
            Arg::Pos(expr) => self.infer_expr(expr, env),
            Arg::Named(named) => self.infer_expr(named.expr(), env),
        }
    }

    fn infer_conditional(
        &mut self,
        conditional: ast::Conditional<'_>,
        env: &mut TyEnv,
        expected: Option<ExpectedTy>,
    ) -> ExprOutcome {
        let mut cond_env = env.child();
        let cond_ty = self.infer_expr(conditional.condition().expr(), &mut cond_env);
        self.emit_eq(
            Ty::Bool,
            cond_ty,
            ConstraintOrigin::Annotation {
                annotated_span: conditional.condition().span(),
            },
            conditional.condition().span(),
        );

        let then_outcome = self.infer_block_outcome_with_expected(
            conditional.consequent().to_untyped(),
            conditional.consequent().span(),
            &mut cond_env,
            expected.as_ref().map(|expected| ExpectedTy {
                ty: expected.ty.clone(),
                origin: ConstraintOrigin::BranchArm {
                    first_arm_span: conditional.consequent().span(),
                    this_arm_span: conditional.consequent().span(),
                },
                span: conditional.consequent().span(),
            }),
        );
        let result_ty = expected.as_ref().map_or_else(
            || {
                if then_outcome.exits {
                    self.var_gen.fresh_ty()
                } else {
                    then_outcome.ty.clone()
                }
            },
            |expected| expected.ty.clone(),
        );
        let mut exits = then_outcome.exits;
        let mut normal_branch_seen = !then_outcome.exits;

        for alternate in conditional.cond_alternates() {
            let mut alt_cond_env = env.child();
            let alt_cond_ty = self.infer_expr(alternate.condition().expr(), &mut alt_cond_env);
            self.emit_eq(
                Ty::Bool,
                alt_cond_ty,
                ConstraintOrigin::Annotation {
                    annotated_span: alternate.condition().span(),
                },
                alternate.condition().span(),
            );

            let alt_outcome = self.infer_block_outcome_with_expected(
                alternate.consequent().to_untyped(),
                alternate.consequent().span(),
                &mut alt_cond_env,
                expected.as_ref().map(|expected| ExpectedTy {
                    ty: expected.ty.clone(),
                    origin: ConstraintOrigin::BranchArm {
                        first_arm_span: conditional.consequent().span(),
                        this_arm_span: alternate.consequent().span(),
                    },
                    span: alternate.consequent().span(),
                }),
            );
            exits &= alt_outcome.exits;
            if expected.is_none() && !alt_outcome.exits {
                normal_branch_seen = true;
                self.emit_eq(
                    result_ty.clone(),
                    alt_outcome.ty,
                    ConstraintOrigin::BranchArm {
                        first_arm_span: conditional.consequent().span(),
                        this_arm_span: alternate.consequent().span(),
                    },
                    alternate.span(),
                );
            }
        }

        if let Some(cond_else) = conditional.cond_else() {
            let else_outcome = self.infer_block_outcome_with_expected(
                cond_else.consequent().to_untyped(),
                cond_else.consequent().span(),
                &mut env.child(),
                expected.as_ref().map(|expected| ExpectedTy {
                    ty: expected.ty.clone(),
                    origin: ConstraintOrigin::BranchArm {
                        first_arm_span: conditional.consequent().span(),
                        this_arm_span: cond_else.consequent().span(),
                    },
                    span: cond_else.consequent().span(),
                }),
            );
            exits &= else_outcome.exits;
            if expected.is_none() && !else_outcome.exits {
                normal_branch_seen = true;
                self.emit_eq(
                    result_ty.clone(),
                    else_outcome.ty,
                    ConstraintOrigin::BranchArm {
                        first_arm_span: conditional.consequent().span(),
                        this_arm_span: cond_else.consequent().span(),
                    },
                    cond_else.span(),
                );
            }
        } else {
            exits = false;
            normal_branch_seen = true;
            self.emit_eq(
                result_ty.clone(),
                Ty::Unit,
                ConstraintOrigin::BranchArm {
                    first_arm_span: conditional.consequent().span(),
                    this_arm_span: conditional.span(),
                },
                conditional.span(),
            );
        }

        let result_ty = if exits && !normal_branch_seen && expected.is_none() {
            Ty::Unit
        } else {
            result_ty
        };

        ExprOutcome {
            ty: result_ty,
            exits,
        }
    }

    fn infer_array(&mut self, array: ast::Array<'_>, env: &mut TyEnv) -> Ty {
        let elem_ty = self.var_gen.fresh_ty();
        for element in array.elements() {
            let actual = self.infer_expr(element, env);
            self.emit_eq(
                elem_ty.clone(),
                actual,
                ConstraintOrigin::BranchArm {
                    first_arm_span: array.span(),
                    this_arm_span: element.span(),
                },
                element.span(),
            );
        }
        self.app_ty("Array", vec![elem_ty])
    }

    fn infer_map(&mut self, map: ast::MapLiteral<'_>, env: &mut TyEnv) -> Ty {
        let value_ty = self.var_gen.fresh_ty();
        for entry in map.entries() {
            if !matches!(entry.key(), Expr::Ident(_)) {
                let key_ty = self.infer_expr(entry.key(), env);
                self.emit_eq(
                    Ty::Str,
                    key_ty,
                    ConstraintOrigin::Annotation {
                        annotated_span: entry.key().span(),
                    },
                    entry.key().span(),
                );
            }
            let actual_value_ty = self.infer_expr(entry.value(), env);
            self.emit_eq(
                value_ty.clone(),
                actual_value_ty,
                ConstraintOrigin::BranchArm {
                    first_arm_span: map.span(),
                    this_arm_span: entry.value().span(),
                },
                entry.value().span(),
            );
        }
        self.app_ty("Map", vec![Ty::Str, value_ty])
    }

    fn infer_lambda(&mut self, lambda: ast::Lambda<'_>, env: &mut TyEnv) -> Ty {
        let mut child = env.child();

        for capture in lambda.captures().children() {
            let Some(capture_symbol) = self.symbol_for_span(capture.binding().span()) else {
                continue;
            };
            let capture_ty = self
                .symbol_table
                .get(capture_symbol)
                .and_then(|symbol| match symbol.symbol_origin {
                    SymbolOrigin::Capture { source, .. } => env.lookup(source).cloned(),
                    _ => None,
                })
                .unwrap_or_else(|| self.var_gen.fresh_ty());
            child.insert(capture_symbol, capture_ty);
        }

        let mut params = Vec::new();
        for param in lambda.params().children() {
            match param.kind() {
                ParamKind::Pos(pattern) => {
                    let ty = self.var_gen.fresh_ty();
                    self.bind_pattern(pattern, ty.clone(), &mut child);
                    params.push(ty);
                }
                ParamKind::Named(named) => {
                    let ty = self.var_gen.fresh_ty();
                    let default_ty = self.infer_expr(named.expr(), env);
                    self.emit_eq(
                        ty.clone(),
                        default_ty,
                        ConstraintOrigin::Annotation {
                            annotated_span: named.name().span(),
                        },
                        named.span(),
                    );
                    if let Some(symbol) = self.symbol_for_span(named.name().span()) {
                        child.insert(symbol, ty.clone());
                    }
                    params.push(ty);
                }
            }
        }

        let return_ty = self.var_gen.fresh_ty();
        let previous_return = self.current_return_ty.replace(return_ty.clone());
        let body_outcome = self.infer_block_outcome_with_expected(
            lambda.to_untyped(),
            lambda.span(),
            &mut child,
            None,
        );
        self.current_return_ty = previous_return;

        let final_return_ty = if body_outcome.exits {
            return_ty
        } else {
            body_outcome.ty
        };

        let fn_ty = Ty::Fn(params, Box::new(final_return_ty));
        self.record_expr(lambda.span(), fn_ty.clone(), fn_ty.clone());
        fn_ty
    }

    fn declare_fn_item_signature(&mut self, item: ast::FnItem<'_>, env: &mut TyEnv) {
        let Some(symbol) = self.symbol_for_span(item.name().span()) else {
            return;
        };

        let mut type_params = HashMap::new();

        for type_arg in item.type_args().items() {
            self.bind_fn_type_param(type_arg, &mut type_params);
        }

        let mut params = Vec::new();
        for param in item.params().children() {
            let ty = param
                .type_annotation()
                .map(|annotation| self.type_from_annotation_with_params(annotation, &type_params))
                .unwrap_or_else(|| self.var_gen.fresh_ty());
            params.push(ty);
        }

        let declared_return_ty = item
            .return_type()
            .map(|annotation| self.type_from_annotation_with_params(annotation, &type_params));
        let return_ty = declared_return_ty
            .clone()
            .unwrap_or_else(|| self.var_gen.fresh_ty());
        let fn_ty = Ty::Fn(params.clone(), Box::new(return_ty.clone()));

        env.insert(symbol, fn_ty.clone());
        self.fn_item_signatures.insert(
            symbol,
            FnItemSignature {
                ty: fn_ty,
                params,
                return_ty,
                declared_return_ty,
            },
        );
    }

    fn infer_fn_item(&mut self, item: ast::FnItem<'_>, env: &mut TyEnv) -> Ty {
        let Some(symbol) = self.symbol_for_span(item.name().span()) else {
            return Ty::Error;
        };

        if !self.fn_item_signatures.contains_key(&symbol) {
            self.declare_fn_item_signature(item, env);
        }

        let Some(signature) = self.fn_item_signatures.get(&symbol).cloned() else {
            return Ty::Error;
        };

        let mut child = env.child();
        child.insert(symbol, signature.ty.clone());

        for (param, ty) in item.params().children().zip(signature.params.iter()) {
            match param.kind() {
                ParamKind::Pos(pattern) => {
                    self.bind_pattern(pattern, ty.clone(), &mut child);
                }
                ParamKind::Named(named) => {
                    let default_ty = self.infer_expr(named.expr(), env);
                    self.emit_eq(
                        ty.clone(),
                        default_ty,
                        ConstraintOrigin::Annotation {
                            annotated_span: named.name().span(),
                        },
                        named.span(),
                    );
                    if let Some(symbol) = self.symbol_for_span(named.name().span()) {
                        child.insert(symbol, ty.clone());
                    }
                }
            }
        }

        let previous_return = self.current_return_ty.replace(signature.return_ty.clone());
        let expected = signature.declared_return_ty.as_ref().map(|ty| ExpectedTy {
            ty: ty.clone(),
            origin: ConstraintOrigin::ReturnType {
                fn_span: item.span(),
                return_expr_span: item.body().span(),
            },
            span: item.body().span(),
        });
        let body_outcome = self.infer_block_outcome_with_expected(
            item.body().to_untyped(),
            item.body().span(),
            &mut child,
            expected,
        );
        self.current_return_ty = previous_return;

        if signature.declared_return_ty.is_none() && !body_outcome.exits {
            self.emit_eq(
                signature.return_ty.clone(),
                body_outcome.ty,
                ConstraintOrigin::ReturnType {
                    fn_span: item.span(),
                    return_expr_span: item.body().span(),
                },
                item.body().span(),
            );
        }

        self.record_expr(item.span(), signature.ty.clone(), signature.ty.clone());
        signature.ty
    }

    fn infer_match(
        &mut self,
        match_expr: ast::MatchExpression<'_>,
        env: &mut TyEnv,
        expected: Option<ExpectedTy>,
    ) -> ExprOutcome {
        let subject_ty = self.infer_expr(match_expr.expr(), env);
        let result_ty = expected
            .as_ref()
            .map_or_else(|| self.var_gen.fresh_ty(), |expected| expected.ty.clone());
        let mut first_arm_span = None;
        let mut saw_arm = false;
        let mut exits = true;
        let mut normal_arm_seen = false;

        for arm in match_expr.match_arms() {
            saw_arm = true;
            let mut arm_env = env.child();
            for pattern in arm.patterns() {
                self.bind_pattern(pattern, subject_ty.clone(), &mut arm_env);
            }
            if let Some(guard) = arm.guard() {
                let guard_ty = self.infer_expr(guard, &mut arm_env);
                self.emit_eq(
                    Ty::Bool,
                    guard_ty,
                    ConstraintOrigin::Annotation {
                        annotated_span: guard.span(),
                    },
                    guard.span(),
                );
            }

            let first_span = *first_arm_span.get_or_insert_with(|| arm.expr().span());
            let arm_outcome = self.infer_expr_outcome_with_expected(
                arm.expr(),
                &mut arm_env,
                expected.as_ref().map(|expected| ExpectedTy {
                    ty: expected.ty.clone(),
                    origin: ConstraintOrigin::BranchArm {
                        first_arm_span: first_span,
                        this_arm_span: arm.expr().span(),
                    },
                    span: arm.expr().span(),
                }),
            );
            exits &= arm_outcome.exits;
            if expected.is_none() && !arm_outcome.exits {
                normal_arm_seen = true;
                self.emit_eq(
                    result_ty.clone(),
                    arm_outcome.ty,
                    ConstraintOrigin::BranchArm {
                        first_arm_span: first_span,
                        this_arm_span: arm.expr().span(),
                    },
                    arm.expr().span(),
                );
            }
        }

        let exits = saw_arm && exits;
        let result_ty = if exits && !normal_arm_seen && expected.is_none() {
            Ty::Unit
        } else {
            result_ty
        };

        ExprOutcome {
            ty: result_ty,
            exits,
        }
    }

    fn infer_block(&mut self, node: &SyntaxNode, span: Span, env: &mut TyEnv) -> Ty {
        self.infer_block_with_expected(node, span, env, None)
    }

    fn infer_block_with_expected(
        &mut self,
        node: &SyntaxNode,
        span: Span,
        env: &mut TyEnv,
        expected: Option<ExpectedTy>,
    ) -> Ty {
        self.infer_block_outcome_with_expected(node, span, env, expected)
            .ty
    }

    fn infer_block_outcome_with_expected(
        &mut self,
        node: &SyntaxNode,
        span: Span,
        env: &mut TyEnv,
        expected: Option<ExpectedTy>,
    ) -> BlockOutcome {
        let fn_items = node
            .children()
            .filter_map(SyntaxNode::cast)
            .collect::<Vec<_>>();
        let statements = statements_with_semicolons(node);
        if fn_items.is_empty() && statements.is_empty() {
            return BlockOutcome {
                ty: Ty::Unit,
                exits: false,
            };
        }

        let mut child = env.child();
        self.declare_fn_items(&fn_items, &mut child);

        for fn_item in fn_items {
            self.infer_fn_item(fn_item, &mut child);
        }

        let mut result = Ty::Unit;
        let mut exits = false;

        for (index, item) in statements.iter().enumerate() {
            let is_tail = index + 1 == statements.len();
            match item.statement {
                Statement::Expr(expr) => {
                    let expr_outcome = if is_tail && item.semicolon.is_none() {
                        self.infer_expr_outcome_with_expected(
                            expr,
                            &mut child,
                            expected.as_ref().map(|expected| ExpectedTy {
                                ty: expected.ty.clone(),
                                origin: expected.origin.clone(),
                                span: expr.span(),
                            }),
                        )
                    } else {
                        self.infer_expr_outcome_with_expected(expr, &mut child, None)
                    };
                    let expr_ty = expr_outcome.ty;
                    if expr_outcome.exits {
                        exits = true;
                        result = Ty::Unit;
                        break;
                    }
                    if is_tail && item.semicolon.is_none() {
                        result = expr_ty;
                    } else {
                        self.record_final(expr.span(), Ty::Unit);
                        if let Some(semi_span) = item.semicolon {
                            if is_tail {
                                self.semicolon_coercions.push(SemicolonCoercion {
                                    block_span: span,
                                    expr_span: expr.span(),
                                    semi_span,
                                    expr_ty: expr_ty.clone(),
                                });
                            }
                            self.constraints.push(Constraint::UnitCoercion {
                                expr_ty,
                                block_ty: Ty::Unit,
                                expr_span: expr.span(),
                                semi_span,
                            });
                        }
                        result = Ty::Unit;
                    }
                }
                statement => {
                    self.infer_statement(statement, &mut child);
                    result = Ty::Unit;
                    if statement_exits(statement) {
                        exits = true;
                        break;
                    }
                }
            }
        }

        if let Some(expected) = &expected {
            if !exits {
                self.emit_eq(
                    expected.ty.clone(),
                    result.clone(),
                    expected.origin.clone(),
                    expected.span,
                );
            }
        }

        self.record_expr(span, result.clone(), result.clone());
        BlockOutcome { ty: result, exits }
    }

    fn bind_pattern(&mut self, pattern: Pattern<'_>, ty: Ty, env: &mut TyEnv) {
        match pattern {
            Pattern::Single(Expr::Ident(ident)) => {
                if let Some(symbol) = self.symbol_for_span(ident.span()) {
                    for var in ty.free_vars() {
                        self.namer.register_source(var, ident.as_str());
                    }
                    env.insert(symbol, ty);
                }
            }
            Pattern::Single(expr) => {
                let expr_ty = self.infer_expr(expr, env);
                self.emit_eq(
                    ty,
                    expr_ty,
                    ConstraintOrigin::Annotation {
                        annotated_span: expr.span(),
                    },
                    expr.span(),
                );
            }
            Pattern::TypedPattern(typed) => {
                let typed_ty = self.type_from_ident(typed.ty());
                self.emit_eq(
                    typed_ty.clone(),
                    ty,
                    ConstraintOrigin::Annotation {
                        annotated_span: typed.ty().span(),
                    },
                    typed.span(),
                );
                self.bind_pattern(typed.pattern(), typed_ty, env);
            }
            Pattern::Destructuring(destructuring) => {
                for binding in destructuring.bindings() {
                    let binding_ty = self.var_gen.fresh_ty();
                    if let Some(symbol) = self.symbol_for_span(binding.span()) {
                        env.insert(symbol, binding_ty);
                    }
                }
            }
            Pattern::LiteralPattern(literal) => {
                if let Some(expr) = Expr::from_untyped(literal.to_untyped()) {
                    let expr_ty = self.infer_expr(expr, env);
                    self.emit_eq(
                        ty,
                        expr_ty,
                        ConstraintOrigin::Annotation {
                            annotated_span: literal.span(),
                        },
                        literal.span(),
                    );
                }
            }
            Pattern::PlaceHolder(_) => {}
        }
    }

    fn implicit_symbol_ty(&mut self, symbol_id: SymbolId) -> Ty {
        let Some(symbol) = self.symbol_table.get(symbol_id) else {
            return Ty::Error;
        };

        match symbol.symbol_kind {
            SymbolKind::Function => self
                .world
                .library()
                .type_info
                .function(symbol.name.as_str())
                .map(|info| info.ty.clone())
                .unwrap_or_else(|| self.var_gen.fresh_ty()),
            SymbolKind::Type => self.app_ty("Type", vec![]),
            SymbolKind::Static | SymbolKind::Module => self
                .world
                .library()
                .global
                .scope()
                .get(symbol.name.as_str())
                .map(|binding| self.ty_from_value(binding.read()))
                .unwrap_or_else(|| self.var_gen.fresh_ty()),
            SymbolKind::Local | SymbolKind::Param => self.var_gen.fresh_ty(),
        }
    }

    fn ty_from_value(&mut self, value: &Value) -> Ty {
        match value {
            Value::Int(_) => Ty::Int,
            Value::Bool(_) => Ty::Bool,
            Value::Unit(_) => Ty::Unit,
            Value::Str(_) => Ty::Str,
            Value::Func(func) => func
                .name()
                .and_then(|name| self.world.library().type_info.function(name))
                .map(|info| info.ty.clone())
                .unwrap_or_else(|| self.var_gen.fresh_ty()),
            Value::Type(_) => self.app_ty("Type", vec![]),
            Value::Iterator(_) => {
                let item = self.var_gen.fresh_ty();
                self.app_ty("Iterator", vec![item])
            }
            Value::Box(_) => {
                let inner = self.var_gen.fresh_ty();
                self.app_ty("Box", vec![inner])
            }
            Value::Array(_) => {
                let elem = self.var_gen.fresh_ty();
                self.app_ty("Array", vec![elem])
            }
            Value::Range(_) => self.app_ty("Range", vec![Ty::Int]),
            Value::Map(_) => {
                let value = self.var_gen.fresh_ty();
                self.app_ty("Map", vec![Ty::Str, value])
            }
            Value::Module(_) => self.app_ty("Module", vec![]),
        }
    }

    fn type_from_ident(&mut self, ident: Ident<'_>) -> Ty {
        self.type_from_name(ident.as_str(), Vec::new())
    }

    fn type_from_annotation(&mut self, annotation: Type<'_>) -> Ty {
        let args = annotation
            .args()
            .map(|args| {
                args.items()
                    .map(|arg| self.type_from_annotation(arg))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_else(Vec::new);
        self.type_from_name(annotation.ident().as_str(), args)
    }

    fn bind_fn_type_param(
        &mut self,
        annotation: Type<'_>,
        type_params: &mut HashMap<SymbolId, Ty>,
    ) {
        if let Some(symbol_id) = self.symbol_for_span(annotation.ident().span()) {
            type_params.entry(symbol_id).or_insert_with(|| {
                let var =
                    self.fresh_named_var(annotation.ident().span(), annotation.ident().as_str());
                Ty::Var(var)
            });
        }

        if let Some(args) = annotation.args() {
            for arg in args.items() {
                self.bind_fn_type_param(arg, type_params);
            }
        }
    }

    fn type_from_annotation_with_params(
        &mut self,
        annotation: Type<'_>,
        type_params: &HashMap<SymbolId, Ty>,
    ) -> Ty {
        if let Some(symbol_id) = self.symbol_for_span(annotation.ident().span()) {
            if let Some(ty) = type_params.get(&symbol_id) {
                return ty.clone();
            }
        }

        let args = annotation
            .args()
            .map(|args| {
                args.items()
                    .map(|arg| self.type_from_annotation_with_params(arg, type_params))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_else(Vec::new);
        self.type_from_name(annotation.ident().as_str(), args)
    }

    fn type_from_name(&mut self, name: &str, args: Vec<Ty>) -> Ty {
        match name {
            "Int" | "int" | "i32" | "i64" if args.is_empty() => Ty::Int,
            "Bool" | "bool" if args.is_empty() => Ty::Bool,
            "Float" | "float" | "f32" | "f64" if args.is_empty() => Ty::Float,
            "Str" | "str" | "String" if args.is_empty() => Ty::Str,
            "Unit" | "unit" if args.is_empty() => Ty::Unit,
            other => self.app_ty(other, args),
        }
    }

    fn app_ty(&mut self, name: &str, args: Vec<Ty>) -> Ty {
        let type_name = if let Some(name) = self.type_names.get(name) {
            name.clone()
        } else {
            let symbol_id = self
                .symbol_table
                .iter()
                .find_map(|(id, symbol)| (symbol.name.as_str() == name).then_some(*id))
                .unwrap_or_else(|| synthetic_symbol_id(name));
            let type_name = TypeName::source(symbol_id);
            self.type_names
                .insert(EcoString::from(name), type_name.clone());
            type_name
        };

        Ty::App(type_name, args)
    }

    fn fresh_named_var(&mut self, span: Span, name: impl Into<String>) -> TypeVar {
        let var = self.var_gen.fresh();
        self.namer.register(var, name);
        self.graph.record_origin(
            var,
            crate::graph::InferenceStep {
                ty: Ty::Var(var),
                span,
                reason: crate::graph::InferenceReason::Annotation,
            },
        );
        var
    }

    fn emit_eq(&mut self, expected: Ty, found: Ty, origin: ConstraintOrigin, span: Span) {
        if found == Ty::Unit {
            self.unit_contexts.push(UnitContext {
                span,
                expected: expected.clone(),
            });
        }

        if let Some(expr_id) = self.expr_ids.get_expr_id(origin.primary_span()) {
            self.constraint_sources
                .entry(expr_id)
                .or_default()
                .push(origin.clone());
        }
        self.constraints.push(Constraint::Eq {
            lhs: expected,
            rhs: found,
            origin,
            span,
        });
    }

    fn record_expr(&mut self, span: Span, original: Ty, final_ty: Ty) {
        let Some(expr_id) = self.expr_ids.get_expr_id(span) else {
            return;
        };
        self.raw_types
            .entry(expr_id)
            .and_modify(|info| {
                info.original = original.clone();
                info.final_ty = final_ty.clone();
            })
            .or_insert(RawTypeInfo { original, final_ty });
    }

    fn record_final(&mut self, span: Span, final_ty: Ty) {
        let Some(expr_id) = self.expr_ids.get_expr_id(span) else {
            return;
        };
        if let Some(info) = self.raw_types.get_mut(&expr_id) {
            info.final_ty = final_ty;
        }
    }

    fn symbol_for_span(&self, span: Span) -> Option<SymbolId> {
        let expr_id = self.expr_ids.get_expr_id(span)?;
        self.expr_to_symbol.get(&expr_id).copied()
    }
}

#[derive(Clone, Copy)]
struct BlockItem<'a> {
    statement: Statement<'a>,
    semicolon: Option<Span>,
}

fn statement_exits(statement: Statement<'_>) -> bool {
    matches!(
        statement,
        Statement::Return(_) | Statement::Break(_) | Statement::Continue(_)
    )
}

fn poisoned_narrowing_spans(errors: &[TypeError]) -> HashSet<Span> {
    errors
        .iter()
        .filter_map(|error| match error {
            TypeError::LiteralTypeMismatch {
                span,
                inference_trail,
                ..
            } => inference_trail
                .iter()
                .rev()
                .find(|entry| {
                    matches!(entry.role, crate::graph::TrailRole::Narrowing { .. })
                        && entry.step.span != *span
                        && matches!(
                            entry.step.reason,
                            crate::graph::InferenceReason::Assignment { .. }
                                | crate::graph::InferenceReason::MethodArgument { .. }
                        )
                })
                .map(|entry| entry.step.span),
            TypeError::Mismatch {
                origin: ConstraintOrigin::Assignment { .. },
                span,
                ..
            } => Some(*span),
            TypeError::Mismatch {
                origin: ConstraintOrigin::FunctionArg { arg_span, .. },
                ..
            } => Some(*arg_span),
            _ => None,
        })
        .collect()
}

fn ty_depends_on_poisoned_narrowing(
    ty: &Ty,
    substitution: &mut Substitution,
    graph: &InferenceGraph,
    poisoned_narrowing_spans: &HashSet<Span>,
) -> bool {
    ty.free_vars().into_iter().any(|var| {
        let canonical = substitution.canonical(var);
        graph
            .collect_trail(canonical)
            .iter()
            .any(|entry| poisoned_narrowing_spans.contains(&entry.step.span))
    })
}

fn statements_with_semicolons(node: &SyntaxNode) -> Vec<BlockItem<'_>> {
    let children = node.children().collect::<Vec<_>>();
    let mut items = Vec::new();

    for (index, child) in children.iter().enumerate() {
        let Some(statement) = child.cast::<Statement<'_>>() else {
            continue;
        };
        let semicolon = children
            .get(index + 1)
            .filter(|next| next.kind() == SyntaxKind::Semicolon)
            .map(|next| next.span());
        items.push(BlockItem {
            statement,
            semicolon,
        });
    }

    items
}

fn assign_op_as_bin_op(op: AssignOp) -> BinOp {
    match op {
        AssignOp::Assign => BinOp::Eq,
        AssignOp::AddAssign => BinOp::Add,
        AssignOp::SubAssign => BinOp::Sub,
        AssignOp::MulAssign => BinOp::Mul,
        AssignOp::DivAssign => BinOp::Div,
        AssignOp::ModAssign => BinOp::Mod,
    }
}

fn diagnostic_to_source(diag: crate::error::Diagnostic) -> SourceDiagnostic {
    let span = diag
        .labels
        .iter()
        .find(|label| label.primary)
        .or_else(|| diag.labels.first())
        .map(|label| label.span)
        .unwrap_or_else(Span::detached);

    let mut source = match diag.severity {
        crate::error::Severity::Error => SourceDiagnostic::error(span, diag.message),
        crate::error::Severity::Warning => SourceDiagnostic::warning(span, diag.message),
    };

    if let Some(primary) = diag.labels.iter().find(|label| label.primary) {
        source = source.with_label_message(primary.message.clone());
    }

    for label in diag.labels.into_iter().filter(|label| !label.primary) {
        source = source.with_label(Label::secondary(label.span, label.message));
    }

    for note in diag.notes {
        source = source.with_note(note);
    }

    if let Some(suggestion) = diag.suggestion {
        source = source.with_hint(suggestion.message);
    }

    source
}

fn diagnostic_dedupe_key(
    diagnostic: &SourceDiagnostic,
) -> ((usize, usize), String, Option<String>) {
    let range = diagnostic
        .span
        .range()
        .map(|range| (range.start, range.end))
        .unwrap_or_default();
    (
        range,
        diagnostic.message.to_string(),
        diagnostic
            .label_message
            .as_ref()
            .map(|message| message.to_string()),
    )
}

fn synthetic_symbol_id(name: &str) -> SymbolId {
    let mut hash = 14_695_981_039_346_656_037u64;
    for byte in name.bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(1_099_511_628_211);
    }
    SymbolId::new(hash.max(1)).expect("hash was forced to be non-zero")
}

#[cfg(test)]
mod tests {
    use super::*;
    use compose_library::Library;
    use compose_library::diag::{FileError, FileResult};
    use compose_resolve::NameResolver;
    use compose_syntax::{FileId, Source};
    use std::collections::HashMap;
    use std::io::{Read, Write};
    use std::sync::Mutex;

    struct TestWorld {
        entrypoint: FileId,
        source: Mutex<HashMap<FileId, Source>>,
        library: Library,
    }

    impl TestWorld {
        fn from_str(text: &str) -> Self {
            let entrypoint = FileId::fake("test.cmps");
            let source = Source::new(entrypoint, text.to_owned());
            let mut sources = HashMap::new();
            sources.insert(entrypoint, source);
            Self {
                entrypoint,
                source: Mutex::new(sources),
                library: Library::default(),
            }
        }

        fn entry_source(&self) -> Source {
            self.source(self.entrypoint).expect("test source exists")
        }
    }

    impl World for TestWorld {
        fn entry_point(&self) -> FileId {
            self.entrypoint
        }

        fn source(&self, file_id: FileId) -> FileResult<Source> {
            self.source
                .lock()
                .expect("source lock")
                .get(&file_id)
                .cloned()
                .ok_or_else(|| FileError::NotFound(file_id.path().0.clone()))
        }

        fn library(&self) -> &Library {
            &self.library
        }

        fn write(
            &self,
            _f: &mut dyn FnMut(&mut dyn Write) -> std::io::Result<()>,
        ) -> std::io::Result<()> {
            Ok(())
        }

        fn read(
            &self,
            _f: &mut dyn FnMut(&mut dyn Read) -> std::io::Result<()>,
        ) -> std::io::Result<()> {
            Ok(())
        }
    }

    fn check_with_resolution(text: &str) -> (TypeCheckResult, ResolutionResult) {
        let world = TestWorld::from_str(text);
        let source = world.entry_source();
        let mut expr_ids = ExprIdTable::new();
        expr_ids.visit_node(source.root_node());

        let mut resolver = NameResolver::new(&expr_ids, &world);
        resolver.set_emit_diagnostics(false);
        let resolution = resolver.resolve();

        (type_check(&world, &expr_ids, &resolution), resolution)
    }

    fn check(text: &str) -> TypeCheckResult {
        check_with_resolution(text).0
    }

    #[test]
    fn catches_assignment_type_conflict() {
        let result = check(
            r#"
            let b;
            b = 4;
            b = true;
            "#,
        );

        assert!(
            !result.diagnostics.is_empty(),
            "expected assignment conflict diagnostic"
        );
    }

    #[test]
    fn assignment_conflict_points_at_later_assignment() {
        let text = r#"
            let mut a = 4;
            a = true;
            "#;
        let result = check(text);

        let diag = result
            .diagnostics
            .iter()
            .find(|diag| diag.message.contains("expected `int`, found `bool`"))
            .unwrap_or_else(|| {
                panic!(
                    "expected int/bool mismatch diagnostic, got {:#?}",
                    result.diagnostics
                )
            });

        assert_eq!(
            diag.span.range().and_then(|range| text.get(range)),
            Some("true"),
            "expected primary diagnostic span to point at `true`, got {diag:#?}"
        );
        assert!(
            diag.labels
                .iter()
                .any(|label| label.message == "`a` was inferred as `int` from this initializer"),
            "expected diagnostic to explain that `a` was inferred from the initializer, got {diag:#?}"
        );
        assert_eq!(
            diag.label_message.as_deref(),
            Some("this assignment gives `a` a `bool`, but `int` was expected"),
            "expected primary label to explain the conflicting assignment, got {diag:#?}"
        );
    }

    #[test]
    fn assignment_inference_label_says_assignment_not_initializer() {
        let text = r#"
            let b;
            b = 4;
            b = true;
            "#;
        let result = check(text);

        let diag = result
            .diagnostics
            .iter()
            .find(|diag| diag.message.contains("expected `int`, found `bool`"))
            .unwrap_or_else(|| {
                panic!(
                    "expected int/bool mismatch diagnostic, got {:#?}",
                    result.diagnostics
                )
            });

        assert!(
            diag.labels
                .iter()
                .any(|label| label.message == "`b` was inferred as `int` from this assignment"),
            "expected diagnostic to explain that `b` was inferred from an assignment, got {diag:#?}"
        );
    }

    #[test]
    fn bad_later_assignment_does_not_poison_earlier_operator_diagnostics() {
        let text = r#"
            let mut a = 4;
            a = 5;

            a = a + 1;
            a = true;
            "#;
        let result = check(text);

        assert!(
            !result
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("operator `+` cannot be applied")),
            "expected no cascaded operator diagnostic, got {:#?}",
            result.diagnostics
        );

        let assignment_mismatches = result
            .diagnostics
            .iter()
            .filter(|diag| diag.message.contains("expected `int`, found `bool`"))
            .collect::<Vec<_>>();
        assert_eq!(
            assignment_mismatches.len(),
            1,
            "expected one assignment mismatch diagnostic, got {:#?}",
            result.diagnostics
        );
        assert_eq!(
            assignment_mismatches[0]
                .span
                .range()
                .and_then(|range| text.get(range)),
            Some("true"),
            "expected mismatch to point at the bad assignment, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn std_println_accepts_variadic_display_arguments() {
        let result = check(r#"println(1, true, "hello");"#);

        assert!(
            result.diagnostics.is_empty(),
            "expected println to accept variadic Display args, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn bare_std_print_functions_have_known_types() {
        let result = check(
            r#"
            println
            print
            "#,
        );

        assert!(
            result.expr_types.values().any(|info| matches!(
                &info.final_ty,
                Ty::VariadicFn { variadic, ret, .. }
                    if matches!(variadic.as_ref(), Ty::Dyn(interface, _) if interface.to_string() == "Display")
                        && ret.as_ref() == &Ty::Unit
            )),
            "expected bare print functions to resolve to variadic Display function types, got {:#?}",
            result.expr_types
        );
        assert!(
            !result
                .expr_types
                .values()
                .any(|info| matches!(info.final_ty, Ty::Var(_))),
            "expected bare print functions not to remain unresolved, got {:#?}",
            result.expr_types
        );
    }

    #[test]
    fn catches_annotation_mismatch() {
        let result = check("let x: Int = true;");

        assert!(
            result
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("mismatched types")),
            "expected annotation mismatch, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn semicolon_preserves_original_and_final_types() {
        let result = check("let x = { 1; };");

        assert!(
            result.diagnostics.is_empty(),
            "discarding a value with `;` should not be a type error: {:#?}",
            result.diagnostics
        );

        assert!(
            result
                .expr_types
                .values()
                .any(|info| info.original == Ty::Int && info.final_ty == Ty::Unit),
            "expected an expression with original int and final unit: {:#?}",
            result.expr_types
        );
    }

    #[test]
    fn trailing_semicolon_mismatch_suggests_removal() {
        let result = check("let x: Int = { 1; };");

        assert!(
            result.diagnostics.iter().any(|diag| diag
                .hints
                .iter()
                .any(|hint| hint.contains("remove the trailing `;`"))),
            "expected trailing semicolon removal hint, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn contextual_type_constrains_if_branches() {
        let result = check(
            r#"
            let x: Int = if (true) {
                1
            } else {
                2
            };
            "#,
        );

        assert!(
            result.diagnostics.is_empty(),
            "expected context to accept both integer branches, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn contextual_if_branch_semicolon_suggests_removal() {
        let result = check(
            r#"
            let x: Int = if (true) {
                1;
            } else {
                2
            };
            "#,
        );

        assert!(
            result.diagnostics.iter().any(|diag| diag
                .hints
                .iter()
                .any(|hint| hint.contains("remove the trailing `;`"))),
            "expected trailing semicolon removal hint for branch context, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn contextual_type_constrains_match_arms() {
        let result = check(
            r#"
            let x: Int = match (true) {
                true => 1,
                false => 2,
            };
            "#,
        );

        assert!(
            result.diagnostics.is_empty(),
            "expected context to accept integer match arms, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn explicit_return_sets_lambda_return_type() {
        let result = check(
            r#"
            let f = { =>
                return 1;
            };
            let x: Int = f();
            "#,
        );

        assert!(
            result.diagnostics.is_empty(),
            "expected explicit return to infer Int without a Unit conflict, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn return_expression_uses_resolved_local_names() {
        let result = check(
            r#"
            let f = { =>
                let x: Int = 1;
                return x;
            };
            let y: Bool = f();
            "#,
        );

        assert!(
            result
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("mismatched types")),
            "expected returned local to constrain the call result, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn fn_item_uses_declared_param_and_return_types() {
        let result = check(
            r#"
            fn add(a: Int, b: Int) -> Int {
                a + b
            }

            let out: Int = add(1, 2);
            "#,
        );

        assert!(
            result.diagnostics.is_empty(),
            "expected fn item call to type check, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn generic_fn_item_type_param_flows_to_call_result() {
        let result = check(
            r#"
            fn id<T>(value: T) -> T {
                value
            }

            let out: Int = id(1);
            "#,
        );

        assert!(
            result.diagnostics.is_empty(),
            "expected generic fn item type param to flow through call, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn fn_item_signature_is_available_before_definition_in_block() {
        let result = check(
            r#"
            let out: Int = {
                let result = add(1, 2);

                fn add(a: Int, b: Int) -> Int {
                    a + b
                }

                result
            };
            "#,
        );

        assert!(
            result.diagnostics.is_empty(),
            "expected fn item to be available throughout its block, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn generic_annotation_constrains_empty_array_element_type() {
        let result = check(
            r#"
            let xs: Array<Int> = [];
            xs.push(1);
            xs.push(false);
            "#,
        );

        assert!(
            result
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("expected `int`, found `bool`")),
            "expected Array<Int> annotation to reject pushing Bool as expected int, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn generic_annotation_resolves_type_arguments() {
        let (_result, resolution) = check_with_resolution("let xs: Array<Nope> = [];");

        assert!(
            resolution
                .unresolved_symbols
                .iter()
                .any(|symbol| symbol.name.as_str() == "Nope"),
            "expected generic type argument to be resolved as a type name"
        );
    }

    #[test]
    fn generic_annotation_resolves_primitive_aliases() {
        let (_result, resolution) = check_with_resolution("let xs: Array<Int> = [];");

        assert!(
            resolution.unresolved_symbols.is_empty(),
            "expected Array<Int> to resolve through primitive aliases, got {:#?}",
            resolution.unresolved_symbols
        );
    }

    #[test]
    fn is_typed_pattern_narrows_binding_in_true_branch() {
        let result = check(
            r#"
            let v;
            let out: Int = if (v is Int s) {
                s + 1
            } else {
                0
            };
            "#,
        );

        assert!(
            result.diagnostics.is_empty(),
            "expected `s` to be narrowed to Int in true branch, got {:#?}",
            result.diagnostics
        );
    }

    #[test]
    fn is_pattern_flow_binding_does_not_escape_else_branch() {
        let (_result, resolution) = check_with_resolution(
            r#"
            let v;
            if (v is Int s) {
                s + 1
            } else {
                s
            };
            "#,
        );

        assert!(
            resolution
                .unresolved_symbols
                .iter()
                .any(|symbol| symbol.name.as_str() == "s"),
            "expected flow binding `s` to be unresolved outside true branch scope"
        );
    }
}
