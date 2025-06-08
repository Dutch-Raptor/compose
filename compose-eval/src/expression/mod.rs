use crate::Eval;
use crate::vm::Vm;
use compose_library::diag::SourceResult;
use compose_library::{UnitValue, Value};
use compose_syntax::ast::{AstNode, Expr};

mod assignment;
mod atomic;
mod binary;
mod bindings;
mod block;
mod call;
mod closure;
mod control_flow;
mod field_access;
mod parenthesized;
mod path_access;
mod unary;

pub use closure::eval_closure;

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
            Expr::Unit(_) => Ok(Value::Unit(UnitValue)),
            Expr::Str(s) => s.eval(vm),
            Expr::Unary(u) => u.eval(vm),
            Expr::Bool(b) => b.eval(vm),
            Expr::FuncCall(f) => f.eval(vm),
            Expr::FieldAccess(f) => f.eval(vm),
            Expr::PathAccess(p) => p.eval(vm),
            Expr::Closure(c) => c.eval(vm),
            Expr::Parenthesized(p) => p.eval(vm),
            Expr::Conditional(c) => c.eval(vm),
            Expr::WhileLoop(w) => w.eval(vm),
            Expr::ForLoop(f) => f.eval(vm),
        }?
        .spanned(span);

        Ok(v)
    }
}