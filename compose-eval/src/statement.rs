use crate::{Eval, Machine};
use compose_library::diag::SourceResult;
use compose_library::Value;
use compose_syntax::ast;

impl Eval for ast::Statement<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Machine) -> SourceResult<Self::Output> {
        let guard = vm.temp_root_guard();
        let result = match self {
            ast::Statement::Expr(e) => e.eval(guard.vm),
            ast::Statement::Let(l) => l.eval(guard.vm),
            ast::Statement::Assign(a) => a.eval(guard.vm),
        };
        guard.vm.maybe_gc();
        result
    }
}
