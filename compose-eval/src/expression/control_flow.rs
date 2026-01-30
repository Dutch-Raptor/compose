use crate::expression::pattern::{destructure_pattern, PatternContext, PatternMatchResult};
use crate::vm::{FlowEvent, Tracked};
use crate::{Eval, Machine};
use compose_library::diag::{At, SourceResult};
use compose_library::{bail, Value};
use compose_library::foundations::iterator::{IterValue, ValueIterator};
use compose_library::foundations::scope::{BindingKind, Visibility};
use compose_syntax::ast;
use compose_syntax::ast::AstNode;
use crate::evaluated::Evaluated;

impl Eval for ast::Conditional<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        {
            let vm_in_flow_scope = &mut vm.new_flow_scope_guard();
            if eval_condition(self.condition().expr(), vm_in_flow_scope)? {
                return self.consequent().eval(vm_in_flow_scope);
            }
        }

        for alternate in self.cond_alternates() {
            let vm_in_flow_scope = &mut vm.new_flow_scope_guard();

            if eval_condition(alternate.condition().expr(), vm_in_flow_scope)? {
                return alternate.consequent().eval(vm_in_flow_scope);
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
        loop {
            let vm_in_flow_scope = &mut vm.new_flow_scope_guard();
            if !eval_condition(self.condition().expr(), vm_in_flow_scope)? {
                break;
            }
            output = self.body().eval(vm_in_flow_scope)?.value;

            match &vm_in_flow_scope.flow {
                None => {}
                Some(FlowEvent::Break(_, value)) => {
                    if let Some(value) = value {
                        output = value.clone();
                    }
                    vm_in_flow_scope.flow = None;
                    break;
                }
                Some(FlowEvent::Continue(_)) => vm_in_flow_scope.flow = None,
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
        let vm = &mut vm.temp_root_guard();
        let mut output = Value::unit();
        let pattern = self.binding();
        let iterable_expr = self.iterable();
        let iterator = {
            let value = iterable_expr.eval(vm)?.track_tmp_root(vm);
            IterValue::try_from_value(value.value, value.mutable, &mut **vm)
                .at(iterable_expr.span())?
                .track_tmp_root(vm)
        };

        let body = self.body();

        let flow = vm.flow.take();

        while let Some(v) = iterator.next(&mut **vm)? {
            vm.in_lexical_scope(|vm| {
                if let PatternMatchResult::NotMatched(err) = destructure_pattern(
                    vm,
                    pattern,
                    v,
                    PatternContext::ForLoopBinding,
                    BindingKind::Immutable { first_assign: None },
                    Visibility::Private,
                )? {
                    bail!(err);
                }

                output = body.eval(vm)?.value;

                SourceResult::Ok(())
            })?;

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

/// Evaluates the condition expression and ensures it's a boolean.
#[inline]
pub fn eval_condition(expr: ast::Expr<'_>, vm: &mut Machine) -> SourceResult<bool> {
    let cond_value = expr.eval(vm)?;
    let as_bool = cond_value.value.cast::<bool>().at(expr.span())?;

    Ok(as_bool)
}
