use std::collections::HashMap;

use crate::{
    graph::{InferenceGraph, InferenceStep},
    ty::{Ty, TypeVar},
};

// ── Union-Find ────────────────────────────────────────────────────────────────
//
// Handles Var ~ Var unification with near-O(1) lookups via path compression.
// Kept strictly separate from the inference graph so path compression never
// mutates data that diagnostic trail collection depends on.

#[derive(Debug, Default)]
struct UnionFind {
    /// parent[v] = the parent of v in the union-find forest.
    /// If parent[v] == v, then v is a root (canonical representative).
    parent: HashMap<TypeVar, TypeVar>,
    /// rank[v] = upper bound on tree height rooted at v.
    rank: HashMap<TypeVar, u32>,
}

impl UnionFind {
    /// Ensure a variable is registered.
    fn make(&mut self, var: TypeVar) {
        self.parent.entry(var).or_insert(var);
        self.rank.entry(var).or_insert(0);
    }

    /// Find the canonical representative for `var`, with path compression.
    fn find(&mut self, var: TypeVar) -> TypeVar {
        self.make(var);
        let parent = self.parent[&var];
        if parent == var {
            return var;
        }
        // Path compression: point directly at the root.
        let root = self.find(parent);
        self.parent.insert(var, root);
        root
    }

    /// Union two variables. Returns (surviving_root, eliminated_var).
    fn union(&mut self, a: TypeVar, b: TypeVar) -> (TypeVar, TypeVar) {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return (ra, ra); // already unified
        }
        // Union by rank: attach smaller tree under larger tree.
        let rank_a = self.rank[&ra];
        let rank_b = self.rank[&rb];
        if rank_a < rank_b {
            self.parent.insert(ra, rb);
            (rb, ra) // rb survives, ra is eliminated
        } else if rank_a > rank_b {
            self.parent.insert(rb, ra);
            (ra, rb) // ra survives, rb is eliminated
        } else {
            self.parent.insert(rb, ra);
            *self.rank.entry(ra).or_insert(0) += 1;
            (ra, rb)
        }
    }
}

// ── Substitution ──────────────────────────────────────────────────────────────

/// Maps type variables to their solutions.
///
/// Architecture:
///   - `union_find`: handles Var ~ Var, gives canonical representatives
///   - `bindings`: maps canonical vars → concrete Ty (non-Var)
///
/// The inference graph is passed in by `&mut` to every mutating operation so
/// that provenance is recorded alongside every structural change.
#[derive(Debug, Default)]
pub struct Substitution {
    union_find: UnionFind,
    /// Only populated when a canonical var is bound to a concrete (non-Var) type.
    bindings: HashMap<TypeVar, Ty>,
}

impl Substitution {
    // ── Querying ──────────────────────────────────────────────────────────────

    /// Fully resolve a type: chase union-find for vars, recurse structurally.
    pub fn apply(&mut self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(v) => {
                let root = self.union_find.find(*v);
                match self.bindings.get(&root).cloned() {
                    Some(bound) => self.apply(&bound),
                    None => Ty::Var(root),
                }
            }
            Ty::App(name, args) => {
                Ty::App(name.clone(), args.iter().map(|a| self.apply(a)).collect())
            }
            Ty::Fn(params, ret) => Ty::Fn(
                params.iter().map(|p| self.apply(p)).collect(),
                Box::new(self.apply(ret)),
            ),
            Ty::VariadicFn {
                params,
                variadic,
                ret,
            } => Ty::VariadicFn {
                params: params.iter().map(|p| self.apply(p)).collect(),
                variadic: Box::new(self.apply(variadic)),
                ret: Box::new(self.apply(ret)),
            },
            Ty::Dyn(iface, args) => {
                Ty::Dyn(iface.clone(), args.iter().map(|a| self.apply(a)).collect())
            }
            other => other.clone(),
        }
    }

    /// Find the canonical representative for a variable (read-only wrapper).
    /// Note: still does path compression internally — that's fine because
    /// the graph uses TypeVar IDs that never change, only the UF routing does.
    pub fn canonical(&mut self, var: TypeVar) -> TypeVar {
        self.union_find.find(var)
    }

    pub fn binding(&self, var: TypeVar) -> Option<&Ty> {
        self.bindings.get(&var)
    }

    // ── Occurs check ─────────────────────────────────────────────────────────

    /// Returns true if `var` appears anywhere in `ty` (after applying subst).
    pub fn occurs(&mut self, var: TypeVar, ty: &Ty) -> bool {
        let root = self.union_find.find(var);
        self.occurs_inner(root, ty)
    }

    fn occurs_inner(&mut self, var: TypeVar, ty: &Ty) -> bool {
        match ty {
            Ty::Var(v) => {
                let root = self.union_find.find(*v);
                if root == var {
                    return true;
                }
                // Check if this var is bound to something that contains `var`.
                if let Some(bound) = self.bindings.get(&root).cloned() {
                    return self.occurs_inner(var, &bound);
                }
                false
            }
            Ty::App(_, args) => args.iter().any(|a| self.occurs_inner(var, a)),
            Ty::Fn(params, ret) => {
                params.iter().any(|p| self.occurs_inner(var, p)) || self.occurs_inner(var, ret)
            }
            Ty::VariadicFn {
                params,
                variadic,
                ret,
            } => {
                params.iter().any(|p| self.occurs_inner(var, p))
                    || self.occurs_inner(var, variadic)
                    || self.occurs_inner(var, ret)
            }
            Ty::Dyn(_, args) => args.iter().any(|a| self.occurs_inner(var, a)),
            _ => false,
        }
    }

    // ── Binding ───────────────────────────────────────────────────────────────

    /// Unify two variables, merging their graph histories.
    /// Returns the surviving canonical representative.
    pub fn union_vars(
        &mut self,
        a: TypeVar,
        b: TypeVar,
        step: InferenceStep,
        graph: &mut InferenceGraph,
    ) -> TypeVar {
        let (survivor, eliminated) = self.union_find.union(a, b);
        if survivor == eliminated {
            graph.record_relation(survivor, step);
            return survivor;
        }

        let survivor_binding = self.bindings.remove(&survivor);
        let eliminated_binding = self.bindings.remove(&eliminated);
        let binding = match (survivor_binding, eliminated_binding) {
            (Some(survivor_binding), Some(eliminated_binding)) => {
                debug_assert_eq!(
                    survivor_binding, eliminated_binding,
                    "conflicting var bindings must be rejected before union"
                );
                Some(survivor_binding)
            }
            (Some(binding), None) | (None, Some(binding)) => Some(binding),
            (None, None) => None,
        };
        if let Some(binding) = binding {
            self.bindings.insert(survivor, binding);
        }

        graph.merge_var_histories(eliminated, survivor);
        graph.record_relation(survivor, step);
        survivor
    }

    /// Bind a canonical variable to a concrete type (non-Var).
    ///
    /// IMPORTANT: Always call this instead of inserting into bindings directly.
    /// The `step` parameter ensures provenance is recorded in the graph.
    ///
    /// Returns Err if the occurs check fails (would create an infinite type).
    pub fn bind_with_evidence(
        &mut self,
        var: TypeVar,
        ty: Ty,
        step: InferenceStep,
        graph: &mut InferenceGraph,
    ) -> Result<(), OccursError> {
        debug_assert!(
            !matches!(ty, Ty::Var(_)),
            "bind_with_evidence called with Var — use union_vars instead"
        );

        let root = self.union_find.find(var);

        // Trivial: already bound to this type.
        if let Some(existing) = self.bindings.get(&root) {
            if existing == &ty {
                return Ok(());
            }
        }

        // Occurs check: binding root := ty would create an infinite type.
        if self.occurs(root, &ty) {
            return Err(OccursError { var: root, ty });
        }

        self.bindings.insert(root, ty.clone());
        graph.record_binding(root, ty, step);
        Ok(())
    }
}

// ── Errors ────────────────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct OccursError {
    pub var: TypeVar,
    pub ty: Ty,
}
