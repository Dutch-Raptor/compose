use compose_library::diag::SourceResult;
use compose_library::Value;
use compose_syntax::ast::{AstNode, Expr};
use crate::Eval;
use crate::vm::Vm;

mod binary;
mod atomic;
mod bindings;

impl Eval for Expr<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let span = self.span();
        let v = match self {
            Expr::Int(i) => i.eval(vm),
            Expr::Binary(b) => b.eval(vm),
            Expr::LetBinding(l) => l.eval(vm),
            Expr::Ident(i) => i.eval(vm),
            v => unimplemented!("cannot eval {v:#?} as it is unimplemented")
        }?;

        // todo: Attach span here
        Ok(v)
    }
}

#[cfg(test)]
pub mod test_utils {
    use compose_library::diag::SourceResult;
    use compose_library::Value;
    use compose_syntax::ast::Expr;
    use compose_syntax::FileId;
    use crate::Eval;
    use crate::vm::Vm;

    pub fn eval_expr(code: &str) -> SourceResult<Value> {
        eval_expr_with_vm(&mut Vm::empty(), code)
    }
    
    pub fn eval_expr_with_vm(vm: &mut Vm, code: &str) -> SourceResult<Value> {
        let file_id = FileId::new("test.comp");
        let nodes = compose_syntax::parse(code, file_id);
        
        let mut value = Value::Unit;
        
        for node in nodes {
            value = node.cast::<Expr>().expect(&format!("{node:#?} is not a valid expression")).eval(vm)?;
        }
        
        Ok(value)
    }
}