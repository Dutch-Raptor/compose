use crate::expression::bindings::destructure_pattern;
use crate::vm::FlowEvent;
use crate::{Eval, Evaluated, Machine};
use compose_library::diag::{At, SourceResult};
use compose_library::{BindingKind, IterValue, Value, ValueIterator};
use compose_syntax::ast;
use compose_syntax::ast::AstNode;

impl Eval for ast::Conditional<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
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

        Ok(Evaluated::unit())
    }
}

impl Eval for ast::WhileLoop<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let mut value = Value::unit();
        while eval_condition(self.condition(), vm)? {
            value = self.body().eval(vm)?.value;
        }
        Ok(Evaluated::mutable(value))
    }
}

impl Eval for ast::ForLoop<'_> {
    //noinspection RsUnnecessaryQualifications - False positive
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let mut output = Value::unit();
        let pattern = self.binding();
        let iterable_expr = self.iterable();
        let iterator = {
            let value = iterable_expr.eval(vm)?;
            IterValue::try_from_value(value.value, value.mutable, vm).at(iterable_expr.span())?
        };
        let body = self.body();

        let flow = vm.flow.take();

        while let Some(v) = iterator.next(vm)? {
            vm.in_scope(|vm| {
                destructure_pattern(
                    vm,
                    pattern,
                    v,
                    BindingKind::Immutable { first_assign: None },
                )?;

                output = body.eval(vm)?.value;

                SourceResult::Ok(())
            })?;
            
            match vm.flow {
                None => {}
                Some(FlowEvent::Break(_)) => {
                    vm.flow = None;
                    break;
                }
                Some(FlowEvent::Continue(_)) => vm.flow = None,
                Some(FlowEvent::Return(..)) => break,
            }
        }

        if let Some(flow) = flow {
            vm.flow = Some(flow);
        }

        Ok(Evaluated::mutable(output))
    }
}

/// Evaluates the condition expression and ensures it's a boolean.
#[inline]
fn eval_condition(cond: ast::Condition<'_>, vm: &mut Machine) -> SourceResult<bool> {
    let cond_expr = cond.expr();
    let cond_value = cond_expr.eval(vm)?;
    cond_value.value.cast::<bool>().at(cond_expr.span())
}
