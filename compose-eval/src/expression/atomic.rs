use crate::vm::Vm;
use crate::Eval;
use compose_library::diag::SourceResult;
use compose_library::Value;
use compose_syntax::ast;

impl Eval for ast::Int<'_> {
    type Output = Value;

    fn eval(self, _vm: &mut Vm) -> SourceResult<Self::Output> {
        Ok(Value::Int(self.get()))
    }
}

impl Eval for ast::Str<'_> {
    type Output = Value;

    fn eval(self, _vm: &mut Vm) -> SourceResult<Self::Output> {
        Ok(Value::Str(self.get().into()))
    }
}

impl Eval for ast::Bool<'_> {
    type Output = Value;
    
    fn eval(self, _vm: &mut Vm) -> SourceResult<Self::Output> {
        Ok(Value::Bool(self.get()))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::assert_eval;
    use super::*;

    #[test]
    fn test_int() {
        assert_eq!(assert_eval("124"), Value::Int(124));
    }
}
