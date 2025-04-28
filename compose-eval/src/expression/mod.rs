mod binary;
mod atomic;


#[cfg(test)]
pub mod test_utils {
    use compose_library::diag::SourceResult;
    use compose_library::Value;
    use compose_syntax::ast::Expr;
    use compose_syntax::FileId;
    use crate::Eval;
    use crate::vm::Vm;

    pub fn eval_expr(code: &str) -> SourceResult<Value> {
        let file_id = FileId::new("test.com");
        let nodes = compose_syntax::parse(code, file_id);
        
        let mut value = Value::Unit;
        let mut vm = Vm::empty();
        
        for node in nodes {
            value = node.cast::<Expr>().unwrap().eval(&mut vm)?;
        }
        
        Ok(value)
    }
}