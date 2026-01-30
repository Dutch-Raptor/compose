use compose_library::engine::Engine;
use compose_library::foundations::args::Args;
use compose_library::foundations::types::Func;
use compose_library::gc::Heap;
use compose_library::Value;
use crate::diag::SourceResult;

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
    #[allow(dead_code)]
    fn dyn_compatible(_vm: &mut dyn Vm) {}
}