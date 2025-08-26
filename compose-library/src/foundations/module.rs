use crate::{Scope, SyntaxContext, Trace};
use compose_library::diag::{bail, error, SourceResult};
use compose_library::{Sink, UntypedRef, Value};
use compose_macros::ty;
use compose_syntax::ast::{AstNode, LetBinding};
use compose_syntax::{FileId, Fix, FixBuilder, Label, Span};
use ecow::EcoString;
use std::sync::Arc;

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
    pub fn path(
        &self,
        path: &str,
        access_span: Span,
        sink: &mut Sink,
        ctx: &SyntaxContext,
    ) -> SourceResult<&Value> {
        let Some(binding) = self.inner.scope.get(path) else {
            bail!(
                access_span,
                "module `{}` has no binding named `{path}`",
                self.name
            );
        };

        if binding.visibility != crate::Visibility::Public {
            let mut err = error!(
                access_span, "cannot access private binding `{path}` in module `{}`", self.name;
                label_message: "this binding is private";
                label: Label::secondary(binding.span(), "declared as private here");
                note: "bindings are private to their module unless explicitly marked `pub`"
            );

            if let Some(fix) = add_pub_fix(binding.span(), ctx) {
                err.add_fix(fix);
            }

            bail!(err);
        }

        Ok(binding.read_checked(access_span, sink))
    }
}

pub fn add_pub_fix(binding_span: Span, ctx: &SyntaxContext) -> Option<Fix> {
    let source = ctx.world.related_source(binding_span)?;
    let node = source.find(binding_span)?;

    let let_binding = node.closest_parent_as::<LetBinding>()?;
    let mut fix_builder = FixBuilder::new(
        "consider marking this binding as public",
        let_binding.span(),
    );

    // Omit long initial values as they do not add any clarity
    if let Some(initial_value) = let_binding.initial_value() {
        if let Some(len) = initial_value.span().len()
            && len > 24
        {
            fix_builder.replace_node(&initial_value, "...");
        }
    }

    fix_builder.insert_before(&let_binding, "pub ");

    Some(fix_builder.build())
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
