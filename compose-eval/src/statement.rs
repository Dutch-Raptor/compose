use crate::{Eval, Machine};
use compose_library::diag::SourceResult;
use compose_library::Value;
use compose_syntax::ast;

impl Eval for ast::Statement<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Machine) -> SourceResult<Self::Output> {
        match self {
            ast::Statement::Expr(e) => e.eval(vm),
            ast::Statement::Let(l) => l.eval(vm),
            ast::Statement::Assign(a) => a.eval(vm),
        }
    }
}
