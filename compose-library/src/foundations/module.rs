use crate::{Scope, Trace};
use compose_library::{Sink, UntypedRef, Value};
use compose_macros::ty;
use compose_syntax::{FileId, Span};
use ecow::EcoString;
use std::sync::Arc;
use compose_library::diag::{bail, StrResult};

#[derive(Debug, Clone)]
#[ty(cast)]
pub struct Module {
    /// The name of the module.
    name: EcoString,
    inner: Arc<Inner>,
}

#[derive(Debug, Clone)]
pub struct Inner {
    /// The scope containing the module's definitions.
    scope: Scope,
    /// The file that defines this module. Or `None` if the module is defined in memory.
    file_id: Option<FileId>,
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.inner.file_id == other.inner.file_id
    }
}

/// path access
impl Module {
    pub fn path(&self, path: &str, access_span: Span, sink: &mut Sink) -> StrResult<&Value> {
        let Some(binding) = self.inner.scope.get(path) else {
            bail!("module `{}` has no binding named `{path}`", self.name);
        };

        Ok(binding.read_checked(access_span, sink))
    }
}

/// Creators
impl Module {
    pub fn new(name: impl Into<EcoString>, scope: Scope) -> Self {
        Self {
            name: name.into(),
            inner: Arc::new(Inner {
                scope,
                file_id: None,
            }),
        }
    }

    pub fn with_file_id(mut self, file_id: FileId) -> Self {
        Arc::make_mut(&mut self.inner).file_id = Some(file_id);
        self
    }

    pub fn with_name(mut self, name: impl Into<EcoString>) -> Self {
        self.name = name.into();
        self
    }

    pub fn with_scope(mut self, scope: Scope) -> Self {
        Arc::make_mut(&mut self.inner).scope = scope;
        self
    }
}

/// Accessors
impl Module {
    pub fn name(&self) -> &EcoString {
        &self.name
    }

    pub fn scope(&self) -> &Scope {
        &self.inner.scope
    }

    pub fn scope_mut(&mut self) -> &mut Scope {
        &mut Arc::make_mut(&mut self.inner).scope
    }

    pub fn file_id(&self) -> Option<FileId> {
        self.inner.file_id
    }
}

impl Trace for Module {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.inner.scope.visit_refs(f);
    }
}
