use std::fmt::Debug;
use compose_library::diag::{At, SourceDiagnostic, SourceResult};
use compose_library::{Binding, IntoValue, Scopes, Sink, Value, World};
use compose_syntax::ast::AstNode;
use compose_syntax::{ast, Span};
use ecow::EcoVec;

pub struct Vm<'a> {
    pub world: &'a dyn World,
    pub scopes: Scopes<'a>,
    pub flow: Option<FlowEvent>,
    pub sink: VmSink,
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

impl<'a> Vm<'a> {
    pub fn new(world: &'a dyn World) -> Self {
        Self {
            world,
            scopes: Default::default(),
            flow: None,
            sink: Default::default(),
        }
    }

    pub fn try_bind(&mut self, var: ast::Ident, binding: Binding) -> SourceResult<()> {
        self.scopes.top.try_bind(var.get().clone(), binding).at(var.span())?;
        
        Ok(())
    }
}

impl Sink for VmSink {
    fn warn(&mut self, warning: SourceDiagnostic) {
        self.warnings.push(warning);
    }
}
