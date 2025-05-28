use compose_library::diag::{bail, At, SourceResult, StrResult};
use compose_library::{ops, Value};
use compose_syntax::ast;
use compose_syntax::ast::{AssignOp, AstNode};
use crate::{Eval, Vm};
use crate::access::Access;

impl Eval for ast::Assignment<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        match self.op() {
            AssignOp::Assign => apply_assignment(self, vm, |_init, rhs| Ok(rhs)),
            AssignOp::AddAssign => apply_assignment(self, vm, ops::add),
            AssignOp::MulAssign => apply_assignment(self, vm, ops::mul),
            other => bail!(self.span(), "unsupported assignment operator: {:?}", other),       
        }
    }
}

fn apply_assignment(
    binary: ast::Assignment,
    vm: &mut Vm,
    op: fn(Value, Value) -> StrResult<Value>,
) -> SourceResult<Value> {
    // assignments are right associative
    let rhs = binary.rhs().eval(vm)?;

    let value_ref = binary.lhs().access(vm)?;
    let lhs = std::mem::take(&mut *value_ref);

    *value_ref = op(lhs, rhs).at(binary.span())?;

    Ok(Value::default())
}
