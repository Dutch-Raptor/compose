use crate::Eval;
use crate::vm::Vm;
use ast::Expr;
use compose_library::Value;
use compose_library::diag::SourceResult;
use compose_syntax::ast;
use compose_syntax::ast::AstNode;

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::empty_world;
    use compose_syntax::ast::Int;
    use compose_syntax::{FileId, parse};

    #[test]
    fn test_int() {
        let file_id = FileId::new("main.comp");
        let node = parse("124", file_id).pop().unwrap();

        let as_int: Int = node.cast().unwrap();

        assert_eq!(as_int.get(), 124);

        let world = empty_world();
        let mut vm = Vm::new(&world);

        assert_eq!(as_int.eval(&mut vm), Ok(Value::Int(124)))
    }
}
