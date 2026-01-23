use crate::{Eval, Machine};
use compose_library::diag::SourceResult;
use compose_syntax::ast;
use crate::evaluated::Evaluated;

impl Eval for ast::Parenthesized<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        self.expr().eval(vm)
    }
}