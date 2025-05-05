use crate::Eval;
use crate::vm::Vm;
use compose_library::Value;
use compose_library::diag::SourceResult;
use compose_syntax::ast::{AstNode, Expr};

mod atomic;
mod binary;
mod bindings;
mod block;

impl Eval for Expr<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let span = self.span();
        let v = match self {
            Expr::Int(i) => i.eval(vm),
            Expr::Binary(b) => b.eval(vm),
            Expr::LetBinding(l) => l.eval(vm),
            Expr::Ident(i) => i.eval(vm),
            Expr::CodeBlock(c) => c.eval(vm),
            Expr::Unit(_) => Ok(Value::Unit),
            Expr::Str(s) => s.eval(vm),
            Expr::Unary(u) => unimplemented!(),
        }?;

        // todo: Attach span here
        Ok(v)
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::Eval;
    use crate::test_utils::test_world;
    use crate::vm::Vm;
    use compose_library::Value;
    use compose_library::diag::SourceResult;
    use compose_syntax::FileId;
    use compose_syntax::ast::Expr;

    pub fn eval_expr(code: &str) -> SourceResult<Value> {
        eval_expr_with_vm(&mut Vm::new(&test_world(code)), code)
    }

    pub fn eval_expr_with_vm(vm: &mut Vm, code: &str) -> SourceResult<Value> {
        let file_id = FileId::new("test.comp");
        let nodes = compose_syntax::parse(code, file_id);

        let mut value = Value::Unit;

        for node in nodes {
            value = node
                .cast::<Expr>()
                .expect(&format!("{node:#?} is not a valid expression"))
                .eval(vm)?;
        }

        Ok(value)
    }
}
