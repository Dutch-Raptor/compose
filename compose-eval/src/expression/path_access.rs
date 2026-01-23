use crate::{Eval, Machine};
use compose_library::diag::SourceResult;
use compose_syntax::ast;
use compose_syntax::ast::AstNode;
use crate::evaluated::Evaluated;

impl Eval for ast::PathAccess<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let target_expr = self.target();
        let member = self.member();

        let target = target_expr.eval(vm)?;
        
        let ctx = vm.syntax_ctx();

        let span = member.span();
        target
            .value
            .path(&member, span, &mut vm.engine.sink, &ctx)
            .map(|v| target.with_value(v))
    }
}
