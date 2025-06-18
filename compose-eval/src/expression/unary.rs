use crate::access::Access;
use crate::{Eval, Machine};
use compose_library::diag::{bail, At, SourceResult, StrResult};
use compose_library::{ops, Heap, Value};
use compose_syntax::ast::{AstNode, UnOp, Unary};

impl Eval for Unary<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Machine) -> SourceResult<Self::Output> {
        let rhs = self.expr().eval(vm)?;

        match self.op() {
            UnOp::Plus => ops::unary_plus(rhs),
            UnOp::Minus => ops::unary_minus(rhs),
            UnOp::Bang => ops::unary_not(rhs),
            UnOp::Tilde => ops::unary_bitwise_not(rhs),
            UnOp::Star => deref(rhs, &vm.heap),
        }
            .at(self.span())
    }
}

fn deref(rhs: Value, heap: &Heap) -> StrResult<Value> {
    match rhs {
        Value::Box(b) => match b.get(heap) {
            Some(value) => Ok(value.clone()),
            None => bail!("Use after free. This is a bug"),
        },
        _ => bail!("only boxed values can be dereferenced. got a {}", rhs.ty()),
    }
}

impl Access for Unary<'_> {
    fn access<'a>(self, vm: &'a mut Machine) -> SourceResult<&'a mut Value> {
        match self.op() {
            UnOp::Star => access_deref(self, vm),
            _ => bail!(
                self.span(),
                "cannot access unary expression with operator {:?}",
                self.op();
                note: "only dereferencing with `*` is supported"
            ),
        }
    }
}

fn access_deref<'a>(unary: Unary, vm: &'a mut Machine) -> SourceResult<&'a mut Value> {
    let span = unary.span();
    let expr = unary.expr();
    let value_ref = expr.access(vm)?;

    if let Value::Box(box_value) = value_ref {
        let heap_ref = box_value.clone();
        match heap_ref.get_mut(&mut vm.heap) {
            Some(v) => Ok(v),
            None => bail!(span, "use after free. this is a bug"),
        }
    } else {
        bail!(span, "cannot deref non-box value");
    }
}
