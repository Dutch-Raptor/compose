use crate::vm::FlowEvent;
use crate::{Eval, Evaluated, Machine};
use compose_library::diag::SourceResult;
use compose_syntax::ast;
use compose_syntax::ast::{AstNode, BreakStatement};

impl Eval for ast::Statement<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        // trace_log!("eval statement: {:#?}", self);
        let guard = vm.temp_root_guard();
        let result = match self {
            ast::Statement::Expr(e) => e.eval(guard.vm),
            ast::Statement::Let(l) => l.eval(guard.vm),
            ast::Statement::Assign(a) => a.eval(guard.vm),
            ast::Statement::Break(b) => b.eval(guard.vm),
            ast::Statement::Return(r) => r.eval(guard.vm),
            ast::Statement::Continue(c) => c.eval(guard.vm),
        };

        guard.vm.maybe_gc();
        result
    }
}

impl Eval for BreakStatement<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        match self.value() {
            None => {
                vm.flow = Some(FlowEvent::Break(self.span(), None));
            }
            Some(expr) => {
                let value = expr.eval(vm)?.value;
                vm.flow = Some(FlowEvent::Break(self.span(), Some(value)))
            }
        }

        Ok(Evaluated::unit())
    }
}

impl Eval for ast::ReturnStatement<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {

        match self.value() {
            None => {
                vm.flow = Some(FlowEvent::Return(self.span(), None));
            }
            Some(expr) => {
                let value = expr.eval(vm)?.value;
                vm.flow = Some(FlowEvent::Return(self.span(), Some(value)))
            }
        }

        Ok(Evaluated::unit())
    }
}

impl Eval for ast::Continue<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        vm.flow = Some(FlowEvent::Continue(self.span()));
        Ok(Evaluated::unit())
    }
}
