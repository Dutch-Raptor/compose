use crate::vm::{Machine, Tracked};
use crate::{Eval, Evaluated};
use compose_library::diag::{At, SourceResult, StrResult, bail};
use compose_library::{Value, ops};
use compose_syntax::ast;
use compose_syntax::ast::{AstNode, BinOp};
use std::ops::Deref;

impl Eval for ast::Binary<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        match self.op() {
            BinOp::Add => apply_binary(self, vm, ops::add),
            BinOp::Sub => apply_binary(self, vm, ops::sub),
            BinOp::Mul => apply_binary(self, vm, ops::mul),
            BinOp::Lt => apply_binary(self, vm, ops::lt),
            BinOp::Gt => apply_binary(self, vm, ops::gt),
            BinOp::Gte => apply_binary(self, vm, ops::gte),
            BinOp::Eq => apply_binary(self, vm, ops::eq),
            BinOp::Neq => apply_binary(self, vm, ops::neq),
            BinOp::And => apply_binary(self, vm, ops::logical_and),
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
    vm: &mut Machine,
    op: fn(&Value, &Value) -> StrResult<Value>,
) -> SourceResult<Evaluated> {
    let l = binary.lhs();
    let lhs = l.eval(vm)?;

    // make sure we dont evaluate rhs when short-circuiting
    if binary.op().short_circuits(&lhs.value) {
        return Ok(Evaluated::mutable(lhs.value));
    }

    let r = binary.rhs();
    let rhs = r.eval(vm)?;

    Ok(Evaluated::mutable(
        op(&lhs.value, &rhs.value).at(binary.span())?,
    ))
}

trait ShortCircuits {
    fn short_circuits(&self, val: &Value) -> bool;
}

impl ShortCircuits for BinOp {
    fn short_circuits(&self, val: &Value) -> bool {
        #[allow(clippy::match_like_matches_macro)]
        match (self, val) {
            (BinOp::And, Value::Bool(false)) => true,
            (BinOp::Or, Value::Bool(true)) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test::assert_eval;
    use compose_library::Value;

    #[test]
    fn test_addition() {
        assert_eq!(assert_eval("2 + 4"), Value::Int(6));
        assert_eq!(assert_eval("2 + 4 + 6"), Value::Int(12));
        assert_eq!(assert_eval("0 + 9884 + 2171"), Value::Int(12055));
    }

    #[test]
    fn test_multiplication() {
        assert_eq!(assert_eval("2 * 4"), Value::Int(8));
        assert_eq!(assert_eval("2 * 4 * 6"), Value::Int(48));
        assert_eq!(assert_eval("0 * 9884 * 2171"), Value::Int(0));
    }

    #[test]
    fn test_mixed() {
        assert_eq!(assert_eval("2 + 4 * 6"), Value::Int(26));
        assert_eq!(assert_eval("2 * 4 + 6"), Value::Int(14));
    }

    #[test]
    fn test_assignment() {
        assert_eq!(assert_eval("let x; x = 6; x"), Value::Int(6));
    }
}
