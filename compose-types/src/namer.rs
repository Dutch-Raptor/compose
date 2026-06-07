use crate::ty::TypeVar;
use std::collections::{HashMap, HashSet};

/// Maps internal TypeVar IDs back to human-readable names for diagnostics.
///
/// Register variables when they are assigned to named bindings during
/// constraint collection. Anonymous generic variables (e.g. the element
/// type of Vec<_>) should be registered as `"_"`.
#[derive(Debug, Default)]
pub struct VarNamer {
    names: HashMap<TypeVar, String>,
    source_names: HashSet<TypeVar>,
}

impl VarNamer {
    /// Register that `var` corresponds to the source name `name`.
    ///
    /// If the variable is already registered, the first registration wins —
    /// the variable identity doesn't change as inference progresses.
    pub fn register(&mut self, var: TypeVar, name: impl Into<String>) {
        self.names.entry(var).or_insert_with(|| name.into());
    }

    /// Register that `var` corresponds to an explicit source binding.
    ///
    /// Source binding names are stronger than descriptive names such as
    /// "integer literal", so they replace an existing non-source name.
    pub fn register_source(&mut self, var: TypeVar, name: impl Into<String>) {
        if self.source_names.insert(var) {
            self.names.insert(var, name.into());
        }
    }

    /// Look up the name for a variable. Falls back to `?T{id}` for anonymous vars.
    pub fn name_for(&self, var: TypeVar) -> String {
        self.names
            .get(&var)
            .cloned()
            .unwrap_or_else(|| format!("?T{}", var.0))
    }

    /// Returns true if this variable has an explicit source name.
    pub fn has_name(&self, var: TypeVar) -> bool {
        self.names.contains_key(&var)
    }

    /// Returns true if this variable is tied to a clear source binding.
    pub fn has_source_name(&self, var: TypeVar) -> bool {
        self.source_names.contains(&var)
    }
}
