use crate::{error::TypeError, subst::Substitution, ty::Ty};
use compose_resolve::ExprId;
use compose_syntax::Span;
use std::collections::HashMap;

/// Apply the final substitution to every node type, replacing all remaining
/// TypeVars with their solutions.
///
/// Any TypeVar that is still unresolved after all solve phases becomes an
/// AmbiguousType error — the user must add an annotation.
pub fn zonk(
    node_tys: &HashMap<ExprId, Ty>,
    node_spans: &HashMap<ExprId, Span>,
    subst: &mut Substitution,
    errors: &mut Vec<TypeError>,
) -> HashMap<ExprId, Ty> {
    let mut result = HashMap::with_capacity(node_tys.len());

    for (&id, ty) in node_tys {
        let resolved = subst.apply(ty);
        let zonked = zonk_ty(resolved, node_spans.get(&id).copied(), errors);
        result.insert(id, zonked);
    }

    result
}

/// Recursively apply the substitution to a single type.
/// Any remaining TypeVar is an ambiguity error.
fn zonk_ty(ty: Ty, span: Option<Span>, errors: &mut Vec<TypeError>) -> Ty {
    match ty {
        Ty::Var(v) => {
            errors.push(TypeError::AmbiguousType {
                var: v,
                span: span.unwrap_or_else(Span::detached),
            });
            Ty::Error
        }
        Ty::App(name, args) => Ty::App(
            name,
            args.into_iter().map(|a| zonk_ty(a, span, errors)).collect(),
        ),
        Ty::Fn(params, ret) => Ty::Fn(
            params
                .into_iter()
                .map(|p| zonk_ty(p, span, errors))
                .collect(),
            Box::new(zonk_ty(*ret, span, errors)),
        ),
        Ty::VariadicFn {
            params,
            variadic,
            ret,
        } => Ty::VariadicFn {
            params: params
                .into_iter()
                .map(|p| zonk_ty(p, span, errors))
                .collect(),
            variadic: Box::new(zonk_ty(*variadic, span, errors)),
            ret: Box::new(zonk_ty(*ret, span, errors)),
        },
        Ty::Dyn(iface, args) => Ty::Dyn(
            iface,
            args.into_iter().map(|a| zonk_ty(a, span, errors)).collect(),
        ),
        other => other,
    }
}
