use crate::IntoValue;
use crate::diag::{At, SourceDiagnostic, SourceResult, error, warning};
use crate::{Library, NativeFuncData, NativeType, Sink, Type, Value};
use compose_error_codes::{E0004_MUTATE_IMMUTABLE_VARIABLE, W0001_USED_UNINITIALIZED_VARIABLE};
use compose_library::diag::{StrResult, bail};
use compose_library::{Func, NativeFunc};
use compose_syntax::{Label, Span};
use dumpster::{Trace, Visitor};
use ecow::{EcoString, eco_format, eco_vec};
use indexmap::IndexMap;
use indexmap::map::Entry;
use std::collections::HashSet;
use std::hash::Hash;
use std::iter;
use strsim::jaro_winkler;
use tap::Pipe;

pub trait NativeScope {
    fn scope() -> Scope;
}

#[derive(Debug, Default, Clone)]
pub struct Scopes<'a> {
    /// The current scope.
    pub top: Scope,
    /// The rest of the scopes.
    pub stack: Vec<Scope>,
    pub lib: Option<&'a Library>,
}

impl<'a> Scopes<'a> {
    pub fn new(lib: Option<&'a Library>) -> Self {
        Self {
            top: Scope::new(),
            stack: Vec::new(),
            lib,
        }
    }

    pub fn enter(&mut self) {
        self.stack.push(std::mem::take(&mut self.top));
    }

    pub fn exit(&mut self) {
        self.top = self.stack.pop().expect("Scope stack underflow");
    }

    pub fn get(&self, name: &str) -> Result<&Binding, VariableAccessError> {
        iter::once(&self.top)
            .chain(self.stack.iter().rev())
            .chain(self.lib.iter().map(|lib| lib.global.scope()))
            .find_map(|scope| scope.get(name))
            .ok_or_else(|| self.unbound_error(name.into()))
    }

    pub fn get_mut(&mut self, name: &str) -> Result<&mut Binding, VariableAccessError> {
        if let Err(err) = self.get(name) {
            return Err(err);
        }

        iter::once(&mut self.top)
            .chain(self.stack.iter_mut().rev())
            .find_map(|scope| scope.get_mut(name))
            .ok_or_else(
                || match self.lib.and_then(|base| base.global.scope().get(name)) {
                    Some(_) => VariableAccessError::MutateConstant(name.into()),
                    None => {
                        debug_assert!(false, "This should have been caught by the get() call");
                        VariableAccessError::Unbound(UnBoundError {
                            name: name.into(),
                            item: UnboundItem::Variable,
                            possible_misspellings: Vec::new(),
                        })
                    }
                },
            )
    }

    fn unbound_error(&self, name: EcoString) -> VariableAccessError {
        let all_idents = iter::once(&self.top)
            .chain(&self.stack)
            .chain(self.lib.iter().map(|lib| lib.global.scope()))
            .flat_map(|scope| scope.map.keys());

        VariableAccessError::Unbound(UnBoundError {
            possible_misspellings: similar_idents(&name, all_idents),
            item: UnboundItem::Variable,
            name,
        })
    }
}

#[derive(Debug, Clone)]
pub enum VariableAccessError {
    Unbound(UnBoundError),
    /// Attempted to mutate a constant. Contains the name of the constant that was attempted to mutate.
    MutateConstant(EcoString),
}

impl<T> At<T> for Result<T, VariableAccessError> {
    fn at(self, span: Span) -> SourceResult<T> {
        self.map_err(|err| match err {
            VariableAccessError::Unbound(err) => {
                eco_vec!(err.to_diag(span))
            }
            VariableAccessError::MutateConstant(name) => eco_vec![
                error!(span, "Cannot mutate constant `{name}`")
                    .with_label_message("is a constant from the standard library")
                    .with_hint("Constants can be shadowed, but not mutated.")
            ],
        })
    }
}

#[derive(Debug, Clone)]
pub struct UnBoundError {
    pub name: EcoString,
    pub item: UnboundItem,
    pub possible_misspellings: Vec<EcoString>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnboundItem {
    Variable,
    /// A field or method (optionally with the name of the type)
    FieldOrMethod(Option<EcoString>),
    AssociatedFieldOrFunction(EcoString),
}

impl UnBoundError {
    pub fn with_item(self, item: UnboundItem) -> Self {
        Self { item, ..self }
    }
    pub fn to_diag(self, span: Span) -> SourceDiagnostic {
        let mut diag = match &self.item {
            UnboundItem::Variable => error!(span, "Unbound variable: `{}`", self.name)
                .with_label_message("this variable is unbound here"),
            UnboundItem::FieldOrMethod(Some(ty)) => error!(
                span,
                "type `{ty}` has no field or method named `{}`", self.name
            )
            .with_label_message(eco_format!(
                "this field or method does not exist on type `{ty}`"
            )),
            UnboundItem::FieldOrMethod(_) => {
                error!(span, "Unbound field or method: `{}`", self.name)
                    .with_label_message("this field or method is unbound here")
            }
            UnboundItem::AssociatedFieldOrFunction(assoc) => error!(
                span,
                "no associated function or field named `{}` found for type `{}`", self.name, assoc
            )
            .with_label_message("unknown function or field"),
        };

        self.apply_hint(&mut diag);
        diag
    }

    pub fn apply_hint(self, diag: &mut SourceDiagnostic) {
        if !self.possible_misspellings.is_empty() {
            diag.hint(eco_format!(
                "Did you mean one of these?\n{}.",
                self.possible_misspellings
                    .iter()
                    .map(|i| eco_format!("  - `{i}`"))
                    .collect::<Vec<_>>()
                    .join("\n"),
            ))
        }
    }
}

impl<T> At<T> for Result<T, UnBoundError> {
    fn at(self, span: Span) -> SourceResult<T> {
        self.map_err(|err| eco_vec!(err.to_diag(span)))
    }
}

#[derive(Debug, Default, Clone)]
pub struct Scope {
    map: IndexMap<EcoString, Binding>,
}

// Safety: Each binding can contain a Gc<T> Value, so each needs to be traced.
// An EcoString cannot contain a Gc<T> Value, so it does not need to be traced.
unsafe impl Trace for Scope {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        for (_k, v) in &self.map {
            v.accept(visitor)?;
        }

        Ok(())
    }
}

impl Scope {
    pub fn try_bind(&mut self, name: EcoString, binding: Binding) -> StrResult<&mut Binding> {
        Ok(match self.map.entry(name) {
            Entry::Occupied(mut entry) => {
                if entry.get().kind == BindingKind::Constant {
                    bail!("Cannot redefine a constant in the same scope");
                }
                entry.insert(binding);
                entry.into_mut()
            }
            Entry::Vacant(entry) => entry.insert(binding),
        })
    }

    pub fn bind(&mut self, name: EcoString, binding: Binding) -> &mut Binding {
        match self.map.entry(name) {
            Entry::Occupied(mut entry) => {
                entry.insert(binding);
                entry.into_mut()
            }
            Entry::Vacant(entry) => entry.insert(binding),
        }
    }

    pub fn define_func<T: NativeFunc>(&mut self) -> &mut Binding {
        let data = T::data();
        self.define(data.name, Func::from(data))
    }

    pub fn define_func_with_data(&mut self, data: &'static NativeFuncData) {
        self.define(data.name, Func::from(data));
    }

    /// Define a variable as a constant value. It cannot be overwritten.
    pub fn define(&mut self, name: &'static str, value: impl IntoValue) -> &mut Binding {
        let binding = Binding::new(value, Span::detached()).with_kind(BindingKind::Constant);
        self.bind(name.into(), binding)
    }

    pub fn define_type<T: NativeType>(&mut self) -> &mut Binding {
        let data = T::data();
        self.define(data.name, Type::from(data))
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

    pub fn try_get(&self, name: &str) -> Result<&Binding, UnBoundError> {
        self.map.get(name).ok_or_else(|| UnBoundError {
            name: name.into(),
            item: UnboundItem::Variable,
            possible_misspellings: self.get_similar_bindings(name),
        })
    }

    pub fn get_similar_bindings(&self, name: &str) -> Vec<EcoString> {
        similar_idents(name, self.map.keys())
    }
}

fn similar_idents<'a>(
    name: &str,
    idents: impl IntoIterator<Item = impl AsRef<str>>,
) -> Vec<EcoString> {
    let mut similar_idents: Vec<_> = idents
        .into_iter()
        .filter_map(|ident| {
            let ident = ident.as_ref();
            let score = jaro_winkler(name, ident);
            (score > 0.8).then(|| (EcoString::from(ident), score))
        })
        .collect();
    similar_idents.sort_unstable_by(|a, b| b.1.total_cmp(&a.1));

    let mut seen = HashSet::new();

    similar_idents
        .into_iter()
        .filter_map(|(ident, _)| {
            if seen.insert(ident.clone()) {
                Some(ident)
            } else {
                None
            }
        })
        .collect()
}

#[derive(Debug, Clone)]
pub struct Binding {
    value: Value,
    span: Span,
    kind: BindingKind,
}

unsafe impl Trace for Binding {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        self.value.accept(visitor)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingKind {
    Variable { first_assign: Option<Span> },
    Mutable,
    Uninitialized,
    UninitializedMutable,
    Constant,
}

impl Binding {
    pub fn new(value: impl IntoValue, span: Span) -> Self {
        Self {
            kind: BindingKind::Variable { first_assign: None },
            value: value.into_value(),
            span,
        }
    }

    pub fn with_kind(self, kind: BindingKind) -> Self {
        Self { kind, ..self }
    }

    pub fn detached(value: impl IntoValue) -> Self {
        Self::new(value, Span::detached())
    }

    pub fn read(&self) -> &Value {
        &self.value
    }

    /// Read the value behind the binding.
    ///
    /// A warning is emitted to the sink if the variable was not yet initialized.
    pub fn read_checked(&self, access_span: Span, sink: &mut Sink) -> &Value {
        if self.is_uninitialized() {
            sink.warn(
                warning!(access_span, "use of variable before it has been initialized";)
                    .with_code(&W0001_USED_UNINITIALIZED_VARIABLE)
                    .with_label_message("use of uninitialized variable")
                    .with_label(Label::secondary(
                        self.span,
                        "variable declared here without an initial value",
                    ))
                    .with_note("uninitialised variables evaluate to `()` by default"),
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
            BindingKind::Variable { first_assign } => Err(eco_vec![
                error!(
                    access_span,
                    "cannot reassign to a variable declared as immutable"
                )
                .with_code(&E0004_MUTATE_IMMUTABLE_VARIABLE)
                .with_label(Label::secondary(self.span, "was defined as immutable here"))
                .pipe(|diag| {
                    if let Some(first_assign) = first_assign {
                        diag.with_label(Label::secondary(
                            first_assign,
                            "first assignment occurred here",
                        ))
                        .with_label_message("cannot reassign an immutable variable")
                    } else {
                        diag.with_label_message("is immutable")
                    }
                })
                .with_note("variables are immutable by default")
                .with_hint("make the variable mutable by writing `let mut`")
            ]),
            BindingKind::Constant => Err(eco_vec![
                error!(access_span, "cannot assign to a constant variable")
                    .with_label_message("is constant")
                    .with_label(Label::secondary(self.span, "was defined as constant here"))
            ]),
            BindingKind::Mutable => Ok(&mut self.value),
            BindingKind::Uninitialized => {
                self.kind = BindingKind::Variable {
                    first_assign: Some(access_span),
                };
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
    
    pub fn is_mutable(&self) -> bool {
        matches!(self.kind, BindingKind::Mutable | BindingKind::UninitializedMutable)
    }
}
