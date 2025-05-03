use compose_library::{Binding, IntoValue, Scopes, Sink, Value};
use std::marker::PhantomData;
use ecow::EcoVec;
use compose_library::diag::SourceDiagnostic;
use compose_syntax::{ast, Span};
use compose_syntax::ast::AstNode;

#[derive(Default, Debug)]
pub struct Vm<'a> {
    phantom_data: PhantomData<&'a ()>,
    pub scopes: Scopes<'a>,
    pub flow: Option<FlowEvent>,
    pub sink: VmSink,
}

#[derive(Default, Debug)]
pub struct VmSink {
    pub warnings: EcoVec<SourceDiagnostic>,
}

#[derive(Debug)]
pub enum FlowEvent {
    Continue(Span),
    Break(Span),
    Return(Span, Option<Value>)
}

impl<'a> Vm<'a> {
    pub fn empty() -> Self {
        Default::default()   
    }
    
    /// Defines an immutable variable in the current scope.
    pub fn define(&mut self, var: ast::Ident, value: impl IntoValue) {
        self.bind(var, Binding::new(value, var.span()))
    }
    
    pub fn define_mutable(&mut self, var: ast::Ident, value: impl IntoValue) {
        self.bind(var, Binding::new_mutable(value, var.span()))
    }
    
    pub fn bind(&mut self, var: ast::Ident, binding: Binding) {
        self.scopes.top.bind(var.get().clone(), binding);
    }
}

impl Sink for VmSink {
    fn warn(&mut self, warning: SourceDiagnostic) {
        self.warnings.push(warning);   
    }
}
