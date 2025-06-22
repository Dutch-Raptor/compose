use crate::diag::{bail, StrResult};
use crate::Value;
use compose_macros::func;
use ecow::EcoString;
use std::io::Write;

mod assertions;

pub use assertions::*;
use compose_library::repr::Repr;
use compose_library::vm::Vm;

#[func]
pub fn panic(msg: Value) -> StrResult<()> {
    bail!("Panic: {:?}", msg)
}

#[func]
pub fn print(vm: &mut dyn Vm, #[variadic] print_args: Vec<Value>) -> StrResult<()> {
    vm.engine()
        .world
        .write(&|wtr: &mut dyn Write| write!(wtr, "{}", join_args(&print_args, vm)))
        .map_err(|e| e.to_string().into())
}
#[func]
pub fn println(vm: &mut dyn Vm, #[variadic] print_args: Vec<Value>) -> StrResult<()> {
    vm.engine()
        .world
        .write(&|wtr: &mut dyn Write| writeln!(wtr, "{}", join_args(&print_args, vm)))
        .map_err(|e| e.to_string().into())
}

fn join_args(args: &[Value], vm: &dyn Vm) -> EcoString {
    let mut joined = EcoString::new();
    for arg in args {
        if !joined.is_empty() {
            joined.push(' ');
        }
        joined.push_str(&arg.repr(vm));
    }
    joined
}
