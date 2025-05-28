use crate::{Eval, Vm};
use compose_library::Value;
use compose_library::diag::SourceResult;
use compose_syntax::ast::CodeBlock;

impl Eval for CodeBlock<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let flow = vm.flow.take();
        vm.scopes.enter();
        let mut result = Value::unit();

        let exprs = self.statements();

        for expr in exprs {
            result = expr.eval(vm)?;
            if vm.flow.is_some() {
                break;
            }
        }

        vm.flow = flow;
        vm.scopes.exit();

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression::test_utils::eval_expr;

    #[test]
    fn test_block() {
        let result = eval_expr(
            r#"
            let a = 4
            let val = {
                let a = 3
                a + 5
            }
            a + val
        "#,
        )
        .expect("eval failed");

        assert_eq!(result, Value::Int(12));
    }

    #[test]
    fn access_outer_scope() {
        let result = eval_expr(
            r#"
            let a = 4
            let val = {
                a + 5
            }
            val
        "#,
        )
        .expect("eval failed");

        assert_eq!(result, Value::Int(9));
    }
}
