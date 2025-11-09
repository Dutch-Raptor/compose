use crate::vm::Machine;
use crate::{Eval, Evaluated, ValueEvaluatedExtensions};
use compose_library::diag::{bail, At, SourceResult};
use compose_library::{ops, Value};
use compose_syntax::ast;
use compose_syntax::ast::{AstNode, BinOp};

impl Eval for ast::Binary<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let op = match self.op() {
            BinOp::Add => ops::add,
            BinOp::Sub => ops::sub,
            BinOp::Mul => ops::mul,
            BinOp::Lt => ops::lt,
            BinOp::Gt => ops::gt,
            BinOp::Gte => ops::gte,
            BinOp::Eq => ops::eq,
            BinOp::Neq => ops::neq,
            BinOp::And => ops::logical_and,
            BinOp::Or => ops::logical_or,
            other => bail!(
                self.span(),
                "unsupported binary operator `{}`",
                other.descriptive_name()
            ),
        };

        let l = self.lhs();
        let lhs = l.eval(vm)?;

        // make sure we dont evaluate rhs when short-circuiting
        if self.op().short_circuits(&lhs.value) {
            return Ok(lhs.value.mutable());
        }

        let r = self.rhs();
        let rhs = r.eval(vm)?;

        Ok(op(&lhs.value, &rhs.value).at(self.span())?.mutable())
    }
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
