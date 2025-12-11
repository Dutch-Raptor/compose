use crate::vm::Tracked;
use crate::{Eval, Evaluated, Machine};
use compose_library::diag::SourceResult;
use compose_library::{IntoValue, Vm};
use compose_syntax::ast;
use compose_syntax::ast::AstNode;

impl Eval for ast::IndexAccess<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let target = self.target().eval(vm)?.track_tmp_root(vm);
        let index = self.index().eval(vm)?.track_tmp_root(vm).value;

        let value =
            target
                .value
                .index(index, self.target().span(), self.index().span(), vm.heap())?;

        Ok(Evaluated::new(value.into_value(), target.mutable))
    }
}
