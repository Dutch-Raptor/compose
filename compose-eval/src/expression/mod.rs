use crate::Eval;
use crate::vm::Machine;
use compose_library::diag::SourceResult;
use compose_syntax::ast::{AstNode, Expr};

mod array;
mod assignment;
mod atomic;
mod binary;
mod bindings;
mod block;
mod call;
mod captures_visitor;
mod closure;
mod control_flow;
mod field_access;
mod index_access;
mod map;
mod match_expression;
mod parenthesized;
mod path_access;
mod pattern;
mod range;
mod unary;

use crate::evaluated::Evaluated;
pub use closure::eval_lambda;

impl Eval for Expr<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let span = self.span();
        let v = match self {
            Expr::Int(i) => i.eval(vm),
            Expr::Binary(b) => b.eval(vm),
            Expr::Ident(i) => i.eval(vm),
            Expr::CodeBlock(c) => c.eval(vm),
            Expr::Unit(_) => Ok(Evaluated::unit()),
            Expr::Str(s) => s.eval(vm),
            Expr::Unary(u) => u.eval(vm),
            Expr::Bool(b) => b.eval(vm),
            Expr::FuncCall(f) => f.eval(vm),
            Expr::FieldAccess(f) => f.eval(vm),
            Expr::PathAccess(p) => p.eval(vm),
            Expr::Parenthesized(p) => p.eval(vm),
            Expr::Conditional(c) => c.eval(vm),
            Expr::WhileLoop(w) => w.eval(vm),
            Expr::ForLoop(f) => f.eval(vm),
            Expr::Array(a) => a.eval(vm),
            Expr::Range(r) => r.eval(vm),
            Expr::Map(m) => m.eval(vm),
            Expr::Lambda(l) => l.eval(vm),
            Expr::IndexAccess(i) => i.eval(vm),
            Expr::MatchExpression(m) => m.eval(vm),
            Expr::IsExpression(i) => i.eval(vm),
        }?
        .spanned(span);

        Ok(v)
    }
}
