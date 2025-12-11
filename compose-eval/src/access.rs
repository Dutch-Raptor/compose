use crate::Eval;
use crate::vm::Machine;
use compose_library::Value;
use compose_library::diag::{At, SourceResult, bail};
use compose_syntax::ast;
use compose_syntax::ast::AstNode;

pub(crate) trait Access {
    /// Returns a mutable reference to the value stored at the given expression.
    ///
    /// Returns an error if the expression is not accessible like a temporary value.
    fn access<'a>(self, vm: &'a mut Machine) -> SourceResult<&'a mut Value>;
}

impl Access for ast::Expr<'_> {
    fn access<'a>(self, vm: &'a mut Machine) -> SourceResult<&'a mut Value> {
        match self {
            Self::Ident(i) => i.access(vm),
            Self::Unary(u) => u.access(vm),
            Self::IndexAccess(i) => i.access(vm),
            Self::Parenthesized(p) => p.access(vm),
            _ => {
                let _ = self.eval(vm)?; // evaluate the expression to make sure errors are reported correctly
                bail!(self.span(), "cannot mutate a temporary value")
            }
        }
    }
}

impl Access for ast::Parenthesized<'_> {
    fn access<'a>(self, vm: &'a mut Machine) -> SourceResult<&'a mut Value> {
        self.expr().access(vm)
    }
}

impl Access for ast::IndexAccess<'_> {
    fn access<'a>(self, vm: &'a mut Machine) -> SourceResult<&'a mut Value> {
        let index_span = self.index().span();
        let index = self.index().eval(vm)?.value;
        let target_span = self.target().span();
        let target = self.target().access(vm)?;

        match target {
            Value::Array(arr) => {
                let index_as_int = index.cast::<usize>().at(index_span)?;

                // We use `heap_ref()` to extract the underlying heap reference without
                // keeping `target` borrowed. This ensures the mutable borrow of `vm` held
                // by `self.target().access(vm)` ends before we borrow `vm.heap` again.
                let arr = arr.heap_ref().get_mut_unwrap(&mut vm.heap);

                let arr_len = arr.len();
                let Some(value) = arr.get_mut(index_as_int) else {
                    bail!(
                        index_span,
                        "index out of bounds: the len is {} but the index is {}",
                        arr_len,
                        index_as_int
                    )
                };

                Ok(value)
            }
            _ => bail!(target_span, "cannot index into `{}`", target.ty()),
        }
    }
}

impl Access for ast::Ident<'_> {
    fn access<'a>(self, vm: &'a mut Machine) -> SourceResult<&'a mut Value> {
        let span = self.span();

        vm.get_mut(&self).at(span)?.write(span)
    }
}
