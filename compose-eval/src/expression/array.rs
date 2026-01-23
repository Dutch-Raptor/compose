use crate::{Eval, Machine};
use compose_library::diag::SourceResult;
use compose_library::{ArrayValue, IntoValue, Value, Vm};
use compose_syntax::ast;
use crate::evaluated::{Evaluated, ValueEvaluatedExtensions};

impl<'a> Eval for ast::Array<'a> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let elements = self.elements();

        let inner = elements
            .map(|e| e.eval(vm).map(|v| v.value))
            .collect::<SourceResult<Vec<Value>>>()?;

        Ok(ArrayValue::from(vm.heap_mut(), inner)
            .into_value()
            .mutable())
    }
}
