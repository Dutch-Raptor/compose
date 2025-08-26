mod stack;

//noinspection RsUnusedImport - false positive, actually used
use crate::expression::eval_lambda;
use crate::vm::stack::{StackFrames, TrackMarker};
use compose_library::diag::{At, SourceDiagnostic, SourceResult, error};
use compose_library::{Args, Binding, BindingKind, Engine, Func, FuncKind, Heap, IntoValue, Routines, Scopes, Sink, Trace, UntypedRef, Value, VariableAccessError, Visibility, Vm, World};
use compose_syntax::ast::AstNode;
use compose_syntax::{Span, ast};
use ecow::EcoString;
pub use stack::Tracked;
pub use stack::TrackedContainer;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};

pub struct Machine<'a> {
    pub frames: StackFrames<'a>,
    pub flow: Option<FlowEvent>,
    pub engine: Engine<'a>,
    pub context: EvalContext,
    pub heap: Heap,
}

impl<'a> Vm<'a> for Machine<'a> {
    fn heap(&self) -> &Heap {
        &self.heap
    }

    fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    fn engine(&self) -> &Engine<'a> {
        &self.engine
    }

    fn engine_mut(&mut self) -> &mut Engine<'a> {
        &mut self.engine
    }

    fn call_func(&mut self, func: &Func, args: Args) -> SourceResult<Value> {
        match &func.kind {
            FuncKind::Native(native) => native.call(self, args),
            FuncKind::Closure(closure) => eval_lambda(closure, self, args),
        }
    }
}

impl<'a> Machine<'a> {
    pub(crate) fn sink_mut(&mut self) -> &mut Sink {
        &mut self.engine.sink
    }

    pub(crate) fn in_scope<T>(&mut self, f: impl FnOnce(&mut Machine<'a>) -> T) -> T {
        self.frames.top.scopes.enter();
        let result = f(self);
        self.frames.top.scopes.exit();
        result
    }

    pub fn world(&self) -> &dyn World {
        self.engine.world
    }
}

impl Debug for Machine<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Vm")
            .field("frames", &self.frames)
            .field("flow", &self.flow)
            .field("sink", &self.engine)
            .field("heap", &self.heap)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum FlowEvent {
    Continue(Span),
    Break(Span, Option<Value>),
    Return(Span, Option<Value>),
}

impl Trace for FlowEvent {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        match self {
            FlowEvent::Continue(_) => {}
            FlowEvent::Break(_, Some(value)) => value.visit_refs(f),
            FlowEvent::Break(_, None) => {}
            FlowEvent::Return(_, Some(value)) => value.visit_refs(f),
            FlowEvent::Return(_, None) => {}
        }
    }
}

impl FlowEvent {
    pub(crate) fn forbidden(&self) -> SourceDiagnostic {
        match *self {
            Self::Break(span, _) => {
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

impl<'a> Machine<'a> {
    pub fn new(world: &'a dyn World) -> Self {
        Self {
            frames: StackFrames::new(Some(world.library())),
            flow: None,
            engine: Engine {
                routines: routines(),
                sink: Sink::default(),
                world,
            },
            context: Default::default(),
            heap: Heap::new(),
        }
    }

    /// Enter a new stack frame to evaluate `f`. This frame does not have access
    /// to any defined variables in other frames, but does share the same heap.
    /// This means references passed into this frame can share data.
    pub fn with_frame<T>(&mut self, f: impl FnOnce(&mut Machine) -> T) -> T {
        self.frames.enter();
        let result = f(self);
        self.frames.exit();
        result
    }

    pub fn maybe_gc(&mut self) {
        let roots = VmRoots {
            frames: &self.frames,
            flow: &self.flow,
        };
        self.heap.maybe_gc(&roots);
    }
}

#[derive(Debug)]
pub struct VmRoots<'a> {
    pub frames: &'a StackFrames<'a>,
    pub flow: &'a Option<FlowEvent>,
}

impl Trace for VmRoots<'_> {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.frames.visit_refs(f);
        if let Some(flow) = self.flow {
            flow.visit_refs(f);
        }
    }
}

impl<'a> Machine<'a> {
    pub fn track_tmp_root(&mut self, value: &impl Trace) {
        self.frames.top.track(value);
    }

    // TODO: remove
    pub fn debug_tracked(&self, from: &str) {
        let mut tracked = Vec::new();
        self
            .frames
            .visit_refs(&mut |k| tracked.push((k, self.heap.get_untyped(k))));
        
        dbg!(from, &tracked);
    }

    pub fn temp_root_marker(&mut self) -> TrackMarker {
        self.frames.top.marker()
    }

    pub fn pop_temp_roots(&mut self, marker: TrackMarker) {
        self.frames.top.forget(marker);
    }

    /// Automatically forgets any tracked values during `f` after f is finished
    pub fn temp_root_scope(
        &mut self,
        f: impl FnOnce(&mut Machine<'a>) -> SourceResult<Value>,
    ) -> SourceResult<Value> {
        let marker = self.temp_root_marker();
        let result = f(self);
        self.pop_temp_roots(marker);
        result
    }

    pub fn define(
        &mut self,
        var: ast::Ident,
        value: impl IntoValue,
        binding_kind: BindingKind,
        visibility: Visibility
    ) -> SourceResult<&mut Binding> {
        self.try_bind(
            var.get().clone(),
            Binding::new(value, var.span()).with_kind(binding_kind).with_visibility(visibility),
        )
    }

    pub fn try_bind(&mut self, name: EcoString, binding: Binding) -> SourceResult<&mut Binding> {
        let span = binding.span();
        self.frames.top.scopes.top.try_bind(name, binding).at(span)
    }

    pub fn bind(&mut self, name: EcoString, binding: Binding) -> &mut Binding {
        self.frames.top.scopes.top.bind(name, binding)
    }

    fn scopes(&self) -> &Scopes<'a> {
        &self.frames.top.scopes
    }
    fn scopes_mut(&mut self) -> &mut Scopes<'a> {
        &mut self.frames.top.scopes
    }

    pub fn get(&self, name: &str) -> Result<&Binding, VariableAccessError> {
        self.scopes().get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Result<&mut Binding, VariableAccessError> {
        self.scopes_mut().get_mut(name)
    }
}

pub struct TempRootGuard<'a, 'b> {
    pub marker: TrackMarker,
    pub vm: &'b mut Machine<'a>,
}

impl<'a, 'b> Drop for TempRootGuard<'a, 'b> {
    fn drop(&mut self) {
        self.vm.pop_temp_roots(self.marker);
    }
}

impl<'a> Machine<'a> {
    pub fn temp_root_guard<'b>(&'b mut self) -> TempRootGuard<'a, 'b> {
        let marker = self.temp_root_marker();
        TempRootGuard { marker, vm: self }
    }
}

impl<'a, 'b> Deref for TempRootGuard<'a, 'b> {
    type Target = Machine<'a>;

    fn deref(&self) -> &Self::Target {
        self.vm
    }
}

impl<'a, 'b> DerefMut for TempRootGuard<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.vm
    }
}

pub fn routines() -> Routines {
    Routines {}
}

#[derive(Debug, Clone, Default)]
pub struct EvalContext {
    pub closure_capture: ErrorMode,
}

/// Whether to defer an error or immediately emit it
#[derive(Debug, Clone, Copy, Default)]
pub enum ErrorMode {
    #[default]
    Immediate,
    Deferred,
}

impl ErrorMode {
    pub fn should_defer(&self) -> bool {
        matches!(self, Self::Deferred)
    }
}

impl Machine<'_> {
    /// Run f with closure capture errors deferred.
    ///
    /// This makes the caller responsible for either handling or emitting the unresolved errors.
    pub fn with_closure_capture_errors_mode<T>(
        &mut self,
        mode: ErrorMode,
        f: impl FnOnce(&mut Machine) -> SourceResult<T>,
    ) -> SourceResult<T> {
        let old = self.context.closure_capture;
        self.context.closure_capture = mode;
        let result = f(self);
        self.context.closure_capture = old;
        result
    }
}
