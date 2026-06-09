use crate::Eval;
use crate::evaluated::{Evaluated, ValueEvaluatedExtensions};
use crate::vm::Machine;
use compose_library::diag::SourceResult;
use compose_library::foundations::cast::IntoValue;
use compose_syntax::ast;

impl Eval for ast::Int<'_> {
    fn eval(self, _vm: &mut Machine) -> SourceResult<Evaluated> {
        Ok(self.get().into_value().mutable())
    }
}

impl Eval for ast::Str<'_> {
    fn eval(self, _vm: &mut Machine) -> SourceResult<Evaluated> {
        Ok(self.get().into_value().mutable())
    }
}

impl Eval for ast::Bool<'_> {
    fn eval(self, _vm: &mut Machine) -> SourceResult<Evaluated> {
        Ok(self.get().into_value().mutable())
    }
}

#[cfg(test)]
mod tests {
    use crate::test::assert_eval;
    use compose_library::Value;

    #[test]
    fn test_int() {
        assert_eq!(assert_eval("124"), Value::Int(124));
    }
}
