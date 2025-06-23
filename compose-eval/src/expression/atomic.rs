use crate::vm::Machine;
use crate::{Eval, Evaluated};
use compose_library::Value;
use compose_library::diag::SourceResult;
use compose_syntax::ast;

impl Eval for ast::Int<'_> {
    fn eval(self, _vm: &mut Machine) -> SourceResult<Evaluated> {
        Ok(Evaluated::mutable(Value::Int(self.get())))
    }
}

impl Eval for ast::Str<'_> {
    fn eval(self, _vm: &mut Machine) -> SourceResult<Evaluated> {
        Ok(Evaluated::mutable(Value::Str(self.get().into())))
    }
}

impl Eval for ast::Bool<'_> {
    fn eval(self, _vm: &mut Machine) -> SourceResult<Evaluated> {
        Ok(Evaluated::mutable(Value::Bool(self.get())))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test::assert_eval;

    #[test]
    fn test_int() {
        assert_eq!(assert_eval("124"), Value::Int(124));
    }
}
