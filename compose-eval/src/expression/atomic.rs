use ast::Expr;
use compose_library::diag::SourceResult;
use compose_library::Value;
use compose_syntax::ast;
use compose_syntax::ast::AstNode;
use crate::Eval;
use crate::vm::Vm;

impl Eval for Expr<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let span = self.span();
        let v = match self {
            Expr::Int(i) => i.eval(vm),
            Expr::Binary(b) => b.eval(vm),
            _ => unimplemented!()
        }?;
        
        // todo: Attach span here
        Ok(v)
    }
}

impl Eval for ast::Int<'_> {
    type Output = Value;

    fn eval(self, _vm: &mut Vm) -> SourceResult<Self::Output> {
        Ok(Value::Int(self.get()))
    }
}


#[cfg(test)]
mod tests {
    use compose_syntax::{parse, FileId};
    use compose_syntax::ast::Int;
    use super::*;
    
    #[test]
    fn test_int() {
        let file_id = FileId::new("main.comp");
        let node = parse(
            "124", file_id
        ).pop().unwrap();
        
        let as_int: Int = node.cast().unwrap();
        
        assert_eq!(as_int.get(), 124);
        assert_eq!(as_int.eval(&mut Vm::empty()), Ok(Value::Int(124)))
    }
}