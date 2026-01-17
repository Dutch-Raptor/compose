use crate::expression::pattern::{PatternMatchResult, destructure_into_flow};
use crate::{Eval, Evaluated, Machine};
use compose_library::Value;
use compose_library::diag::{SourceResult, bail};
use compose_library::repr::Repr;
use compose_syntax::ast::{AstNode, MatchExpression, Pattern};
use compose_utils::trace_fn;

impl Eval for MatchExpression<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        trace_fn!("eval_match_expression");
        let vm = &mut vm.new_flow_scope_guard();
        let value = self.expr().eval(vm)?.value;

        'arms: for arm in self.match_arms() {
            trace_fn!("eval_match_arm");
            let vm_in_arm_flow_scope = &mut vm.new_flow_scope_guard();

            if !try_patterns(&value, vm_in_arm_flow_scope, arm.patterns())? {
                continue 'arms;
            }

            if let Some(guard) = arm.guard() {
                match guard.eval(vm_in_arm_flow_scope)?.value {
                    Value::Bool(true) => {} // matched, carry on evaluating the arm
                    // Continue matching arms if the guard evaluates to false.
                    Value::Bool(false) => continue 'arms,
                    other => {
                        bail!(guard.span(), "match guard must evaluate to a boolean";
                        label_message: "guard evaluated to `{}`", other.repr(&mut **vm_in_arm_flow_scope);
                        note: "expected `Bool`, found `{}`", other.ty();
                        hint: "consider removing the guard to match any value")
                    }
                }
            }

            let result = arm.expr().eval(vm_in_arm_flow_scope)?;

            // exit scope
            return Ok(result);
        }

        // no matches
        bail!(self.span(), "no match arm applies to this value";
            note: "the value: `{}` is not covered by any match arm", value.repr(&mut **vm);
            hint: "consider adding a default `_ => ...` arm to handle all remaining cases"
        )
    }
}

fn try_patterns<'a>(
    value: &Value,
    vm: &mut Machine,
    patterns: impl IntoIterator<Item = Pattern<'a>>,
) -> SourceResult<bool> {
    for pattern in patterns {
        match destructure_into_flow(vm, value.clone(), pattern)? {
            PatternMatchResult::Matched => {
                return Ok(true);
            }
            PatternMatchResult::NotMatched(_) => {
                continue;
            }
        }
    }

    Ok(false)
}
