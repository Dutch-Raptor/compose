use compose_library::diag::{bail, At, SourceResult, StrResult};
use compose_library::{ops, Value, ValueRef};
use compose_syntax::ast;
use compose_syntax::ast::{AssignOp, AstNode};
use crate::{Eval, Vm};
use crate::access::Access;

impl Eval for ast::Assignment<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        match self.op() {
            AssignOp::Assign => apply_assignment(self, vm, |_init, rhs| Ok(rhs.clone())),
            AssignOp::AddAssign => apply_assignment(self, vm, ops::add),
            AssignOp::SubAssign => apply_assignment(self, vm, ops::sub),
            AssignOp::MulAssign => apply_assignment(self, vm, ops::mul),
            other => bail!(self.span(), "unsupported assignment operator: {:?}", other),       
        }
    }
}

fn apply_assignment(
    binary: ast::Assignment,
    vm: &mut Vm,
    op: fn(ValueRef, ValueRef) -> StrResult<Value>,
) -> SourceResult<Value> {
    // assignments are right associative
    let rhs = binary.rhs().eval(vm)?;

    let lhs = binary.lhs().access(vm)?;
    let lhs_ref = lhs.as_ref().at(binary.lhs().span())?;
    let rhs_ref = rhs.as_ref().at(binary.rhs().span())?;

    let value = op(lhs_ref, rhs_ref).at(binary.span())?;
    let mut dst = lhs.as_mut().at(binary.lhs().span())?;
    *dst = value;

    Ok(Value::default())
}
