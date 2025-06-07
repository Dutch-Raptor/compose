use crate::access::Access;
use crate::{Eval, Vm};
use compose_library::diag::{bail, At, SourceResult, StrResult};
use compose_library::{ops, Value};
use compose_syntax::ast;
use compose_syntax::ast::{AssignOp, AstNode};
use std::ops::Deref;

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
    op: fn(&Value, &Value) -> StrResult<Value>,
) -> SourceResult<Value> {
    // assignments are right associative
    let rhs = binary.rhs().eval(vm)?;
    let rhs_ref = rhs.as_ref().at(binary.rhs().span())?;

    let mut lhs = binary.lhs().access(vm)?;
    let value = op(lhs.deref(), rhs_ref.deref()).at(binary.span())?;
    *lhs = value;

    Ok(Value::default())
}
