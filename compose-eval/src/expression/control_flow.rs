use crate::expression::bindings::destructure_pattern;
use crate::vm::{FlowEvent, Tracked};
use crate::{Eval, Evaluated, Machine};
use compose_library::diag::{At, SourceResult};
use compose_library::{BindingKind, IterValue, Value, ValueIterator, Visibility};
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
        let mut output = Value::unit();
        let flow = vm.flow.take();
        while eval_condition(self.condition(), vm)? {
            output = self.body().eval(vm)?.value;

            match &vm.flow {
                None => {}
                Some(FlowEvent::Break(_, value)) => {
                    if let Some(value) = value {
                        output = value.clone();
                    }
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

impl Eval for ast::ForLoop<'_> {
    //noinspection RsUnnecessaryQualifications - False positive
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let root_guard = vm.temp_root_guard();
        let mut output = Value::unit();
        let pattern = self.binding();
        let iterable_expr = self.iterable();
        let iterator = {
            let value = iterable_expr
                .eval(root_guard.vm)?
                .track_tmp_root(root_guard.vm);
            IterValue::try_from_value(value.value, value.mutable, root_guard.vm)
                .at(iterable_expr.span())?
                .track_tmp_root(root_guard.vm)
        };

        let body = self.body();

        let flow = root_guard.vm.flow.take();

        while let Some(v) = iterator.next(root_guard.vm)? {
            root_guard.vm.in_scope(|vm| {
                destructure_pattern(
                    vm,
                    pattern,
                    v,
                    BindingKind::Immutable { first_assign: None },
                    Visibility::Private,
                )?;

                output = body.eval(vm)?.value;

                SourceResult::Ok(())
            })?;

            match &root_guard.vm.flow {
                None => {}
                Some(FlowEvent::Break(_, value)) => {
                    if let Some(value) = value {
                        output = value.clone();
                    }
                    root_guard.vm.flow = None;
                    break;
                }
                Some(FlowEvent::Continue(_)) => root_guard.vm.flow = None,
                Some(FlowEvent::Return(..)) => break,
            }
        }

        if let Some(flow) = flow {
            root_guard.vm.flow = Some(flow);
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
