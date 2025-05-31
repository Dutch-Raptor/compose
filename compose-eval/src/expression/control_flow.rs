use crate::{Eval, Vm};
use compose_library::diag::{At, SourceResult};
use compose_library::Value;
use compose_syntax::ast;
use compose_syntax::ast::AstNode;

impl Eval for ast::Conditional<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        if eval_condition(self.condition(), vm)? {
            return self.consequent().eval(vm);
        } 
         
        for alternate in self.cond_alternates() {
            if eval_condition(alternate.condition(), vm)? {
                return alternate.consequent().eval(vm);
            } 
        }
        
        if let Some(cond_else) = self.cond_else() {
            return cond_else.consequent().eval(vm);
        }
        
        Ok(Value::unit())
    }
}

/// Evaluates the condition expression and ensures it's a boolean.
#[inline]
fn eval_condition(cond: ast::Condition<'_>, vm: &mut Vm) -> SourceResult<bool> {
    let cond_expr = cond.expr();
    let cond_value = cond_expr.eval(vm)?;
    cond_value.cast::<bool>().at(cond_expr.span())
}