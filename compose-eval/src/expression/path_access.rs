use crate::{Eval, Vm};
use compose_library::diag::SourceResult;
use compose_library::Value;
use compose_syntax::ast;
use compose_syntax::ast::AstNode;

impl Eval for ast::PathAccess<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let target_expr = self.target();
        let member = self.member();

        let target = target_expr.eval(vm)?;

        let span = member.span();
        target.path(&member, span, vm.sink_mut())
    }
}
