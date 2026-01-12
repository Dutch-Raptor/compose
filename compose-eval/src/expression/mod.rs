use crate::vm::{Machine, Tracked};
use crate::{Eval, Evaluated};
use compose_library::diag::{SourceResult};
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
mod array;
mod range;
mod map;
mod index_access;
mod pattern;
mod match_expression;

pub use closure::eval_lambda;

impl Eval for Expr<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let span = self.span();
        let v = match self {
            Expr::Int(i) => i.eval(vm),
            Expr::Binary(b) => b.eval(vm),
            Expr::LetBinding(l) => l.eval(vm),
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
        }?
        .spanned(span)
        .track_tmp_root(vm);

        Ok(v)
    }
}
