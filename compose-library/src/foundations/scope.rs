use crate::diag::{error, warning, SourceResult, StrResult};
use crate::{IntoValue, Sink, Value};
use compose_syntax::Span;
use ecow::{eco_format, eco_vec, EcoString};
use indexmap::map::Entry;
use indexmap::IndexMap;
use std::marker::PhantomData;

#[derive(Debug, Default, Clone)]
pub struct Scopes<'a> {
    phantom_data: PhantomData<&'a ()>,
    /// The current scope.
    pub top: Scope,
    /// The rest of the scopes.
    pub stack: Vec<Scope>,
}

impl<'a> Scopes<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn enter(&mut self) {
        self.stack.push(std::mem::take(&mut self.top));
    }

    pub fn exit(&mut self) {
        self.top = self.stack.pop().expect("Scope stack underflow");
    }

    pub fn get(&self, name: &str) -> StrResult<&Binding> {
        std::iter::once(&self.top)
            .chain(self.stack.iter().rev())
            .find_map(|scope| scope.get(name))
            .ok_or_else(|| error!("Unbound variable: {name}"))
    }

    pub fn get_mut(&mut self, name: &str) -> StrResult<&mut Binding> {
        std::iter::once(&mut self.top)
            .chain(self.stack.iter_mut().rev())
            .find_map(|scope| scope.get_mut(name))
            .ok_or_else(|| error!("Unbound variable: {name}"))
    }
}

#[derive(Debug, Default, Clone)]
pub struct Scope {
    map: IndexMap<EcoString, Binding>,
}

impl Scope {
    pub fn bind(&mut self, name: EcoString, binding: Binding) -> &mut Binding {
        match self.map.entry(name) {
            Entry::Occupied(mut entry) => {
                entry.insert(binding);
                entry.into_mut()
            }
            Entry::Vacant(entry) => entry.insert(binding),
        }
    }
}

impl Scope {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, name: &str) -> Option<&Binding> {
        self.map.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Binding> {
        self.map.get_mut(name)
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    value: Value,
    span: Span,
    kind: BindingKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingKind {
    Immutable,
    Mutable,
    Uninitialized,
    UninitializedMutable,
}

impl Binding {
    pub fn new(value: impl IntoValue, span: Span) -> Self {
        Self {
            kind: BindingKind::Immutable,
            value: value.into_value(),
            span,
        }
    }

    pub fn with_kind(self, kind: BindingKind) -> Self {
        Self { kind, ..self }
    }

    pub fn new_mutable(value: impl IntoValue, span: Span) -> Self {
        Self {
            kind: BindingKind::Mutable,
            value: value.into_value(),
            span,
        }
    }

    pub fn detached(value: impl IntoValue) -> Self {
        Self::new(value, Span::detached())
    }

    pub fn detached_mutable(value: impl IntoValue) -> Self {
        Self::new_mutable(value, Span::detached())
    }

    pub fn read(&self) -> &Value {
        &self.value
    }

    /// Read the value behind the binding.
    ///
    /// A warning is emitted to the sink if the variable was not yet initialized.
    pub fn read_checked(&self, access_span: Span, sink: &mut impl Sink) -> &Value {
        if self.is_uninitialized() {
            sink.warn(
                warning!(access_span, "Read an uninitialised variable"; 
                hint: "Uninitialised variables are always `()`.";)
                    .with_label_message("was uninitialised here")
                .with_label(self.span, "was defined here without an initial value"),
            )
        }
        self.read()
    }

    /// Get a mutable reference to the value behind the binding
    ///
    /// Returns an error if the value is not mutable.
    ///
    /// If the binding was not yet initialized, its kind will be updated to the corresponding
    /// initialized kind.
    pub fn write(&mut self, access_span: Span) -> SourceResult<&mut Value> {
        match self.kind {
            BindingKind::Immutable => Err(eco_vec![
                error!(access_span, "Cannot assign to an immutable variable more than once")
                .with_label_message("is immutable")
                    .with_label(self.span, "was defined as immutable here")
            ]),
            BindingKind::Mutable => Ok(&mut self.value),
            BindingKind::Uninitialized => {
                self.kind = BindingKind::Immutable;
                Ok(&mut self.value)
            }
            BindingKind::UninitializedMutable => {
                self.kind = BindingKind::Mutable;
                Ok(&mut self.value)
            }
        }
    }

    pub fn is_uninitialized(&self) -> bool {
        matches!(
            self.kind,
            BindingKind::Uninitialized | BindingKind::UninitializedMutable
        )
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn kind(&self) -> BindingKind {
        self.kind
    }
}
