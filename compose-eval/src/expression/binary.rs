use crate::Eval;
use crate::vm::Vm;
use compose_library::diag::{At, SourceResult, StrResult, bail};
use compose_library::{Value, ops};
use compose_syntax::ast;
use compose_syntax::ast::{AstNode, BinOp};

impl Eval for ast::Binary<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        match self.op() {
            BinOp::Add => apply_binary(self, vm, ops::add),
            BinOp::Mul => apply_binary(self, vm, ops::mul),
            BinOp::Lt => apply_binary(self, vm, ops::lt),
            other => bail!(
                self.span(),
                "unsupported binary operator `{}`",
                other.descriptive_name()
            ),
        }
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
    use crate::expression::test_utils::eval_code;
    use compose_library::Value;

    #[test]
    fn test_addition() {
        assert_eq!(eval_code("2 + 4"), Ok(Value::Int(6)));
        assert_eq!(eval_code("2 + 4 + 6"), Ok(Value::Int(12)));
        assert_eq!(eval_code("0 + 9884 + 2171"), Ok(Value::Int(12055)));
    }

    #[test]
    fn test_multiplication() {
        assert_eq!(eval_code("2 * 4"), Ok(Value::Int(8)));
        assert_eq!(eval_code("2 * 4 * 6"), Ok(Value::Int(48)));
        assert_eq!(eval_code("0 * 9884 * 2171"), Ok(Value::Int(0)));
    }

    #[test]
    fn test_mixed() {
        assert_eq!(eval_code("2 + 4 * 6"), Ok(Value::Int(26)));
        assert_eq!(eval_code("2 * 4 + 6"), Ok(Value::Int(14)));
    }

    #[test]
    fn test_assignment() {
        assert_eq!(eval_code("let x; x = 6; x"), Ok(Value::Int(6)));
    }
}
