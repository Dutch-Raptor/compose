use crate::access::Access;
use crate::{Eval, Machine};
use compose_library::diag::{bail, At, SourceResult, StrResult};
use compose_library::{Value};
use compose_library::foundations::ops;
use compose_library::gc::Heap;
use compose_syntax::ast::{AstNode, UnOp, Unary};
use crate::evaluated::Evaluated;

impl Eval for Unary<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let rhs = self.expr().eval(vm)?;

        match self.op() {
            UnOp::Plus => ops::unary_plus(rhs.value).map(Evaluated::mutable),
            UnOp::Minus => ops::unary_minus(rhs.value).map(Evaluated::mutable),
            UnOp::Bang => ops::unary_not(rhs.value).map(Evaluated::mutable),
            UnOp::Tilde => ops::unary_bitwise_not(rhs.value).map(Evaluated::mutable),
            UnOp::Star => deref(rhs, &vm.heap),
        }
        .at(self.span())
    }
}

fn deref(rhs: Evaluated, heap: &Heap) -> StrResult<Evaluated> {
    match rhs.value {
        Value::Box(b) => match b.get(heap) {
            Some(value) => Ok(Evaluated::new(value.clone(), rhs.mutable)),
            None => bail!("Use after free. This is a bug"),
        },
        _ => bail!(
            "only boxed values can be dereferenced. got a {}",
            rhs.value.ty()
        ),
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
