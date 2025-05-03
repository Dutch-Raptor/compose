mod vm;
mod expression;
mod access;

use compose_library::diag::SourceResult;
use crate::vm::Vm;

pub trait Eval {
    type Output;
    
    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output>;
}