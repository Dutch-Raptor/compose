use compose_library::diag::{At, SourceDiagnostic, SourceResult, error};
use compose_library::{
    Binding, BindingKind, Engine, IntoValue, Routines, Scopes, Sink, Value, World,
};
use compose_syntax::ast::AstNode;
use compose_syntax::{Span, ast};
use ecow::EcoString;
use std::fmt::Debug;

pub struct Vm<'a> {
    pub scopes: Scopes<'a>,
    pub flow: Option<FlowEvent>,
    pub engine: Engine<'a>,
    pub context: EvalContext
}


impl<'a> Vm<'a> {
    pub(crate) fn sink_mut(&mut self) -> &mut Sink {
        &mut self.engine.sink
    }

    pub(crate) fn in_scope<T>(&mut self, f: impl FnOnce(&mut Vm<'a>) -> T) -> T {
        self.scopes.enter();
        let result = f(self);
        self.scopes.exit();
        result
    }

    pub fn world(&self) -> &dyn World {
        self.engine.world
    }
}

impl Debug for Vm<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Vm")
            .field("scopes", &self.scopes)
            .field("flow", &self.flow)
            .field("sink", &self.engine)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum FlowEvent {
    Continue(Span),
    Break(Span),
    Return(Span, Option<Value>),
}

impl FlowEvent {
    pub(crate) fn forbidden(&self) -> SourceDiagnostic {
        match *self {
            Self::Break(span) => {
                error!(span, "cannot break outside of a loop")
            }
            Self::Return(span, _) => {
                error!(span, "cannot return outside of a function")
            }
            Self::Continue(span) => {
                error!(span, "cannot continue outside of a loop")
            }
        }
    }
}

impl<'a> Vm<'a> {
    pub fn new(world: &'a dyn World) -> Self {
        Self {
            scopes: Scopes::new(Some(world.library())),
            flow: None,
            engine: Engine {
                routines: routines(),
                sink: Sink::default(),
                world,
            },
            context: Default::default(),
        }
    }

    pub fn define(
        &mut self,
        var: ast::Ident,
        value: impl IntoValue,
        binding_kind: BindingKind,
    ) -> SourceResult<()> {
        self.try_bind(
            var.get().clone(),
            Binding::new(value, var.span()).with_kind(binding_kind),
        )?;
        Ok(())
    }

    pub fn try_bind(&mut self, name: EcoString, binding: Binding) -> SourceResult<&mut Binding> {
        let span = binding.span();
        self.scopes.top.try_bind(name, binding).at(span)
    }
}

pub fn routines() -> Routines {
    Routines {
        eval_closure: crate::expression::eval_closure,
    }
}

#[derive(Debug, Clone, Default)]
pub struct EvalContext {
    pub closure_capture: DeferredErrorMode,
}

/// Whether to defer an error or immediately emit it
#[derive(Debug, Clone, Copy, Default)]
pub enum DeferredErrorMode {
    #[default]
    Immediate,
    Deferred,
}

impl DeferredErrorMode {
    pub fn should_defer(&self) -> bool {
        matches!(self, Self::Deferred)
    }
}

impl Vm<'_> {
    /// Run f with closure capture errors deferred.
    ///
    /// This makes the caller responsible for either handling or emitting the unresolved errors.
    pub fn with_deferred_closure_capture_errors<T>(&mut self, f: impl FnOnce(&mut Vm) -> SourceResult<T>) -> SourceResult<T> {
        let old = self.context.closure_capture;
        self.context.closure_capture = DeferredErrorMode::Deferred;
        let result = f(self);
        self.context.closure_capture = old;
        result
    }
}
