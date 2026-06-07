use crate::ty::Ty;
use compose_resolve::SymbolId;
use std::collections::HashMap;

/// A scoped mapping from variable names to their types.
///
/// Clone to enter a new scope; discard the clone when exiting.
/// This is intentionally simple — in a production compiler you'd use
/// a persistent data structure or a scope stack to avoid cloning.
#[derive(Debug, Clone, Default)]
pub struct TyEnv {
    vars: HashMap<SymbolId, Ty>,
}

impl TyEnv {
    pub fn lookup(&self, name: SymbolId) -> Option<&Ty> {
        self.vars.get(&name)
    }

    pub fn insert(&mut self, name: SymbolId, ty: Ty) {
        self.vars.insert(name, ty);
    }

    /// Create a child scope that inherits all bindings of the parent.
    pub fn child(&self) -> Self {
        self.clone()
    }
}
