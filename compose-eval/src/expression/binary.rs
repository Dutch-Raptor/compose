use crate::vm::Machine;
use crate::{Eval, Evaluated, ValueEvaluatedExtensions};
use compose_library::diag::{At, SourceResult, bail};
use compose_library::{Value, ops};
use compose_syntax::ast;
use compose_syntax::ast::{AstNode, BinOp};

impl Eval for ast::Binary<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let op = match self.op() {
            BinOp::Add => |l, r, _heap| ops::add(l, r),
            BinOp::Sub => |l, r, _heap| ops::sub(l, r),
            BinOp::Mul => |l, r, _heap| ops::mul(l, r),
            BinOp::Lt => |l, r, _heap| ops::lt(l, r),
            BinOp::Gt => |l, r, _heap| ops::gt(l, r),
            BinOp::Gte => |l, r, _heap| ops::gte(l, r),
            BinOp::Eq => |l, r, heap| ops::eq(l, r, heap),
            BinOp::Neq => |l, r, heap| ops::neq(l, r, heap),
            BinOp::Mod => |l, r, _heap| ops::mod_(l, r),
            BinOp::And => |l, r, _heap| ops::logical_and(l, r),
            BinOp::Or => |l, r, _heap| ops::logical_or(l, r),
            other => bail!(
                self.span(),
                "unsupported binary operator `{}`",
                other.descriptive_name()
            ),
        };

        // The lhs expression might introduce flow bindings used in rhs,
        // we make sure to run in a flow scope that last at least as long
        // as this entire binary expression
        let vm = &mut vm.in_flow_scope_guard();

        let l = self.lhs();
        let lhs = l.eval(vm)?;

        // make sure we dont evaluate rhs when short-circuiting
        if self.op().short_circuits(&lhs.value) {
            return Ok(lhs.value.mutable());
        }

        let r = self.rhs();
        let rhs = r.eval(vm)?;

        Ok(op(&lhs.value, &rhs.value, &vm.heap)
            .at(self.span())?
            .mutable())
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
