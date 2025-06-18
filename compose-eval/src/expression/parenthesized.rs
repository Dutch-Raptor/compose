use crate::{Eval, Machine};
use compose_library::diag::SourceResult;
use compose_library::Value;
use compose_syntax::ast;

impl Eval for ast::Parenthesized<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Machine) -> SourceResult<Self::Output> {
        self.expr().eval(vm)
    }
}