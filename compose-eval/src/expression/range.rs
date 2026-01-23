use crate::{Eval, Machine};
use compose_library::diag::{At, SourceResult};
use compose_library::{RangeValue, Value};
use compose_syntax::ast;
use compose_syntax::ast::AstNode;
use crate::evaluated::Evaluated;

impl Eval for ast::Range<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let start = self
            .start()
            .map(|e| e.eval(vm))
            .transpose()?
            .map(Evaluated::into_value);
        let end = self
            .end()
            .map(|e| e.eval(vm))
            .transpose()?
            .map(Evaluated::into_value);
        let include_end = self.is_inclusive();

        Ok(Evaluated::mutable(Value::Range(
            RangeValue::new(start, end, include_end).at(self.span())?,
        )))
    }
}
