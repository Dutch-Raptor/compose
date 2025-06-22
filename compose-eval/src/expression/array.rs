use crate::{Eval, Machine};
use compose_library::diag::SourceResult;
use compose_library::{ArrayValue, Value, Vm};
use compose_syntax::ast;

impl<'a> Eval for ast::Array<'a> {
    type Output = Value;

    fn eval(self, vm: &mut Machine) -> SourceResult<Self::Output> {
        let elements = self.elements();

        let inner = elements
            .map(|e| e.eval(vm))
            .collect::<SourceResult<Vec<Value>>>()?;

        Ok(Value::Array(ArrayValue::from(vm.heap_mut(), inner)))
    }
}
