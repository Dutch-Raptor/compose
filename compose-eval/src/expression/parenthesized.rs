use compose_library::diag::SourceResult;
use compose_library::Value;
use compose_syntax::ast;
use crate::{Eval, Vm};

impl Eval for ast::Parenthesized<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        self.expr().eval(vm)
    }
}