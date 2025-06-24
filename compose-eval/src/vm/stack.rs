use crate::Machine;
use compose_library::{Binding, Library, Scopes, Trace, UntypedRef, VariableAccessError};

#[derive(Debug, Clone)]
pub struct StackFrames<'a> {
    pub(crate) top: StackFrame<'a>,
    frames: Vec<StackFrame<'a>>,
    library: Option<&'a Library>,
}

impl<'a> StackFrames<'a> {
    pub fn new(library: Option<&'a Library>) -> Self {
        Self {
            top: StackFrame::new(library),
            frames: vec![],
            library,
        }
    }

    pub fn enter(&mut self) {
        let new_frame = StackFrame::new(self.library);
        self.frames
            .push(std::mem::replace(&mut self.top, new_frame))
    }

    pub fn exit(&mut self) {
        self.top = self.frames.pop().expect("Frame stack underflow");
    }

    pub fn get(&self, name: &str) -> Result<&Binding, VariableAccessError> {
        self.top.scopes.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Result<&mut Binding, VariableAccessError> {
        self.top.scopes.get_mut(name)
    }
}

impl Trace for StackFrames<'_> {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        self.top.visit_refs(f);
        for frame in &self.frames {
            frame.visit_refs(f);
        }
        if let Some(lib) = self.library {
            lib.visit_refs(f)
        }
    }
}

#[derive(Debug, Clone)]
pub struct StackFrame<'a> {
    pub scopes: Scopes<'a>,
    tracked: Vec<UntypedRef>,
}

impl<'a> StackFrame<'a> {
    fn new(library: Option<&'a Library>) -> StackFrame<'a> {
        Self {
            scopes: Scopes::new(library),
            tracked: vec![],
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TrackMarker(usize);

impl<'a> StackFrame<'a> {
    pub fn track(&mut self, value: &impl Trace) {
        value.visit_refs(&mut |key| self.tracked.push(key));
    }

    pub fn marker(&self) -> TrackMarker {
        TrackMarker(self.tracked.len())
    }

    pub fn forget(&mut self, marker: TrackMarker) {
        self.tracked.truncate(marker.0);
    }
}

impl Trace for StackFrame<'_> {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        for ref_ in &self.tracked {
            f(*ref_);
        }
        self.scopes.visit_refs(f);
    }
}

pub trait Tracked {
    /// Temporarily track this value as a GC Root
    fn track_tmp_root(self, vm: &mut Machine) -> Self;
}

impl<T> Tracked for T
where
    T: Trace,
{
    
    fn track_tmp_root(self, vm: &mut Machine) -> Self {
        vm.track_tmp_root(&self);
        self
    }
}

pub trait TrackedContainer {
    fn track_tmp_root(self, vm: &mut Machine) -> Self;
}

impl<T, E> TrackedContainer for Result<T, E>
where
    T: Tracked,
{
    fn track_tmp_root(self, vm: &mut Machine) -> Self {
        self.map(|x| x.track_tmp_root(vm))
    }
}

impl<T> TrackedContainer for Option<T>
where
    T: Tracked,
{
    fn track_tmp_root(self, vm: &mut Machine) -> Self {
        self.map(|x| x.track_tmp_root(vm))
    }
}
