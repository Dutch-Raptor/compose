use crate::{Eval, Machine};
use compose_library::diag::SourceResult;
use compose_library::Value;
use compose_syntax::ast::CodeBlock;

impl Eval for CodeBlock<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Machine) -> SourceResult<Self::Output> {
        let flow = vm.flow.take();
        let mut result = Value::unit();

        let statements = self.statements();

        vm.in_scope(|vm| {
            for statement in statements {
                result = statement.eval(vm)?;
                if vm.flow.is_some() {
                    break;
                }
            }
            SourceResult::Ok(())
        })?;

        if let Some(flow) = flow {
            vm.flow = Some(flow);
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test::assert_eval;

    #[test]
    fn test_block() {
        let result = assert_eval(
            r#"
            let a = 4;
            let val = {
                let a = 3;
                a + 5;
            };
            a + val;
        "#,
        );

        assert_eq!(result, Value::Int(12));
    }

    #[test]
    fn access_outer_scope() {
        let result = assert_eval(
            r#"
            let a = 4;
            let val = {
                a + 5;
            };
            val;
        "#,
        );

        assert_eq!(result, Value::Int(9));
    }
}
