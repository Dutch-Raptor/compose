use crate::expression::eval_closure;
use compose_library::diag::{error, At, SourceDiagnostic, SourceResult};
use compose_library::{Binding, IntoValue, Routines, Scopes, Sink, Value, World};
use compose_syntax::ast::AstNode;
use compose_syntax::{ast, Span};
use ecow::EcoVec;
use std::fmt::Debug;

pub struct Vm<'a> {
    pub world: &'a dyn World,
    pub scopes: Scopes<'a>,
    pub flow: Option<FlowEvent>,
    pub sink: VmSink,
    pub routines: Routines,
}

impl Debug for Vm<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Vm")
            .field("scopes", &self.scopes)
            .field("flow", &self.flow)
            .field("sink", &self.sink)
            .finish()
    }
}

#[derive(Default, Debug)]
pub struct VmSink {
    pub warnings: EcoVec<SourceDiagnostic>,
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
            world,
            scopes: Scopes::new(Some(world.library())),
            flow: None,
            sink: Default::default(),
            routines: routines(),
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
    Routines { eval_closure }
}

impl Sink for VmSink {
    fn warn(&mut self, warning: SourceDiagnostic) {
        self.warnings.push(warning);
    }
}
