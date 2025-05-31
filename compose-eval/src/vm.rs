use compose_library::diag::{error, At, SourceDiagnostic, SourceResult};
use compose_library::{Binding, Engine, IntoValue, Routines, Scopes, Sink, Value, World};
use compose_syntax::ast::AstNode;
use compose_syntax::{ast, Span};
use std::fmt::Debug;

pub struct Vm<'a> {
    pub scopes: Scopes<'a>,
    pub flow: Option<FlowEvent>,
    pub engine: Engine<'a>,
}

impl<'a> Vm<'a> {
    pub(crate) fn sink_mut(&mut self) -> &mut Sink {
        &mut self.engine.sink
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


#[derive(Debug)]
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
        }
    }

    pub fn define(&mut self, var: ast::Ident, value: impl IntoValue) -> SourceResult<()> {
        self.try_bind(var, Binding::new(value, var.span()))
    }

    pub fn try_bind(&mut self, var: ast::Ident, binding: Binding) -> SourceResult<()> {
        self.scopes
            .top
            .try_bind(var.get().clone(), binding)
            .at(var.span())?;

        Ok(())
    }
}

pub fn routines() -> Routines {
    Routines {
        eval_closure: crate::expression::eval_closure,
    }
}