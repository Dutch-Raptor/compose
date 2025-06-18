use crate::diag::SourceResult;
use crate::{Args, Engine};
use compose_library::{Func, Heap, Value};

pub trait Vm<'a> {
    fn heap(&self) -> &Heap;
    fn heap_mut(&mut self) -> &mut Heap;

    fn engine(&self) -> &Engine<'a>;
    fn engine_mut(&mut self) -> &mut Engine<'a>;

    fn call_func(&mut self, func: &Func, args: Args) -> SourceResult<Value>;
}

#[cfg(test)]
mod tests {
    use compose_library::Vm;

    /// Make sure that Vm is dyn compatible.
    fn dyn_compatible(vm: &mut dyn Vm) {}
}