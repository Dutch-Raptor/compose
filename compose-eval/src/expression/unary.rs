use compose_library::diag::{At, SourceResult};
use compose_library::{ops, Value};
use compose_syntax::ast::{AstNode, UnOp, Unary};
use crate::{Eval, Vm};

impl Eval for Unary<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        
        let rhs = self.expr().eval(vm)?;
        
        match self.op() {
            UnOp::Plus => ops::unary_plus(rhs),
            UnOp::Minus => ops::unary_minus(rhs),
            UnOp::Bang => ops::unary_not(rhs),
            UnOp::Tilde => ops::unary_bitwise_not(rhs),
        }.at(self.span())
    }
}