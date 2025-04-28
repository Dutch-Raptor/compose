use crate::vm::Vm;
use crate::Eval;
use compose_library::diag::{At, SourceResult, StrResult};
use compose_library::Value;
use compose_syntax::ast;
use compose_syntax::ast::{AstNode, BinOp};

impl Eval for ast::Binary<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        match self.op() {
            BinOp::Add => apply_binary(self, vm, add),
            other => unimplemented!("{:?}", other),
        }
    }
}

fn add(lhs: Value, rhs: Value) -> StrResult<Value> {
    match (lhs, rhs) {
        (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left + right)),
        (left, right) => unimplemented!("{:?} + {:?}", left, right),
    }
}

fn apply_binary(
    binary: ast::Binary,
    vm: &mut Vm,
    op: fn(Value, Value) -> StrResult<Value>,
) -> SourceResult<Value> {
    let lhs = binary.lhs().eval(vm)?;

    // make sure we dont evaluate rhs when short-circuiting
    if binary.op().short_circuits(&lhs) {
        return Ok(lhs);
    }
    
    let rhs = binary.rhs().eval(vm)?;
    op(lhs, rhs).at(binary.span())
}

trait ShortCircuits {
    fn short_circuits(&self, val: &Value) -> bool;
}
impl ShortCircuits for BinOp {
    fn short_circuits(&self, val: &Value) -> bool {
        match (self, val) {
            (BinOp::And, Value::Bool(false)) => true,
            (BinOp::Or, Value::Bool(true)) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::expression::test_utils::eval_expr;
    use compose_library::Value;

    #[test]
    fn test_addition() {
        assert_eq!(eval_expr("2 + 4"), Ok(Value::Int(6)));
        assert_eq!(eval_expr("2 + 4 + 6"), Ok(Value::Int(12)));
        assert_eq!(eval_expr("0 + 9884 + 2171"), Ok(Value::Int(12055)));
    }
}
