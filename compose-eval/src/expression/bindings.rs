use crate::vm::Vm;
use crate::Eval;
use compose_library::diag::{At, SourceResult};
use compose_library::{diag, Binding, Value};
use compose_syntax::ast;
use compose_syntax::ast::{AstNode, Expr, Pattern};
use ecow::eco_vec;

impl<'a> Eval for ast::Ident<'a> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let span = self.span();
        Ok(vm
            .scopes
            .get(&self)
            .at(span)?
            .read_checked(span, &mut vm.sink)
            .clone())
    }
}

impl<'a> Eval for ast::LetBinding<'a> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let init = self.initial_value();

        let binding_kind = match (self.is_mut(), init) {
            (true, Some(_)) => compose_library::BindingKind::Mutable,
            (false, Some(_)) => compose_library::BindingKind::Immutable { first_assign: None },
            (true, None) => compose_library::BindingKind::UninitializedMutable,
            (false, None) => compose_library::BindingKind::Uninitialized,
        };

        let value = match init {
            Some(expr) => expr.eval(vm)?,
            None => Value::unit(),
        };

        // handle control flow
        if vm.flow.is_some() {
            return Ok(Value::unit());
        }

        destructure_pattern(vm, self.pattern(), value, binding_kind)?;

        Ok(Value::unit())
    }
}

fn destructure_pattern(
    vm: &mut Vm,
    pattern: Pattern,
    value: Value,
    binding_kind: compose_library::BindingKind,
) -> SourceResult<()> {
    destructure_impl(vm, pattern, value, &mut |vm, expr, value| match expr {
        Expr::Ident(ident) => {
            vm.try_bind(
                ident,
                Binding::new(value, ident.span()).with_kind(binding_kind),
            )?;
            Ok(())
        }
        _ => Err(eco_vec![diag::SourceDiagnostic::error(
            expr.span(),
            "cannot destructure pattern",
        )]),
    })
}

fn destructure_impl(
    vm: &mut Vm,
    pattern: Pattern,
    value: Value,
    bind: &mut impl Fn(&mut Vm, Expr, Value) -> SourceResult<()>,
) -> SourceResult<()> {
    match pattern {
        Pattern::Single(expr) => bind(vm, expr, value)?,
        Pattern::PlaceHolder(_) => {} // A placeholder means we discard the value, no need to bind
        Pattern::Destructuring(destruct) => {
            unimplemented!("destructuring")
        }
    };

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression::test_utils::eval_expr_with_vm;
    use compose_library::{BindingKind, UnitValue};
    use crate::test_utils::test_world;

    #[test]
    fn test_let_binding() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        eval_expr_with_vm(&mut vm, "let a = 3").expect("failed to evaluate");

        let binding = vm.scopes.top.get("a").unwrap();

        assert_eq!(binding.read(), &Value::Int(3));
        assert_eq!(binding.kind(), BindingKind::Immutable { first_assign: None });
    }

    #[test]
    fn test_let_mut_binding() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        eval_expr_with_vm(&mut vm, "let mut a = 3").expect("failed to evaluate");

        let binding = vm.scopes.top.get("a").unwrap();

        assert_eq!(binding.read(), &Value::Int(3));
        assert_eq!(binding.kind(), BindingKind::Mutable);
    }

    #[test]
    fn test_let_binding_without_value() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        eval_expr_with_vm(&mut vm, "let a").expect("failed to evaluate");

        let binding = vm.scopes.top.get("a").unwrap();

        assert_eq!(binding.read(), &Value::unit());
        assert_eq!(binding.kind(), BindingKind::Uninitialized);
    }

    #[test]
    fn test_let_mut_binding_without_value() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        eval_expr_with_vm(&mut vm, "let mut a").expect("failed to evaluate");

        let binding = vm.scopes.top.get("a").unwrap();
        assert_eq!(binding.read(), &Value::unit());
        assert_eq!(binding.kind(), BindingKind::UninitializedMutable);
    }

    #[test]
    fn test_read_ident() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        // define the variable
        eval_expr_with_vm(&mut vm, "let a = 3").expect("failed to evaluate");
        // read the variable
        let result = eval_expr_with_vm(&mut vm, "a").expect("failed to evaluate");
        assert_eq!(result, Value::Int(3));
    }

    #[test]
    fn test_read_uninitialised_variable() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        // set up the variable
        eval_expr_with_vm(&mut vm, "let a").expect("failed to evaluate");
        let result = eval_expr_with_vm(&mut vm, "a").expect("failed to evaluate");
        assert_eq!(result, Value::unit());

        // should have emitted a warning
        assert_eq!(vm.sink.warnings.len(), 1);
        let warning = &vm.sink.warnings[0];
        assert!(warning.message.contains("uninitialised"));
        // should have a hint
        assert_eq!(warning.hints.len(), 1);
        assert!(warning.hints[0].contains("Uninitialised variables are always `()`"));
    }
    
    #[test]
    fn integration() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        let result = eval_expr_with_vm(&mut vm, r#"
            let a = 3
            let b = 4
            let c = a + b
            c * 2
        "#).expect("failed to evaluate");
        
        assert_eq!(result, Value::Int(14));
    }
    
    #[test]
    fn assign_mut() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        eval_expr_with_vm(&mut vm, "let mut a = 3").expect("failed to evaluate");
        
        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Mutable);
        
        eval_expr_with_vm(&mut vm, "a = 4").expect("failed to evaluate");
        let result = eval_expr_with_vm(&mut vm, "a").expect("failed to evaluate");
        assert_eq!(result, Value::Int(4));
        
        // should still be mutable
        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Mutable);
    }
    
    #[test]
    fn assign_mut_uninitialised() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        eval_expr_with_vm(&mut vm, "let mut a").expect("failed to evaluate");
        
        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::UninitializedMutable);
        assert_eq!(binding.read(), &Value::Unit(UnitValue));
        
        eval_expr_with_vm(&mut vm, "a = 4").expect("failed to evaluate");
        let result = eval_expr_with_vm(&mut vm, "a").expect("failed to evaluate");
        assert_eq!(result, Value::Int(4));
        
        // should now be mutable
        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Mutable);
    }
    
    #[test]
    fn assign_uninitialised() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        eval_expr_with_vm(&mut vm, "let a").expect("failed to evaluate");
        
        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Uninitialized);
        assert_eq!(binding.read(), &Value::unit());
        
        eval_expr_with_vm(&mut vm, "a = 4").expect("failed to evaluate");
        let result = eval_expr_with_vm(&mut vm, "a").expect("failed to evaluate");
        assert_eq!(result, Value::Int(4));
        
        // should now be immutable
        let binding = vm.scopes.get("a").unwrap();
        assert!(matches!(binding.kind(), BindingKind::Immutable {..}));
    }
    
    #[test]
    fn assign_immut_error() {
        let world = test_world("");
        let mut vm = Vm::new(&world);
        eval_expr_with_vm(&mut vm, "let a = 3").expect("failed to evaluate");
        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Immutable { first_assign: None });
        assert_eq!(binding.read(), &Value::Int(3));
        
        let errs = eval_expr_with_vm(&mut vm, "a = 4").expect_err("expected error");
        
        assert_eq!(errs.len(), 1);
        let err = &errs[0];
        assert!(err.message.contains("Cannot mutate an immutable variable"));
        assert!(err.hints.is_empty());
        
        assert_eq!(err.labels.len(), 1);
        let label = &err.labels[0];
        assert!(label.message.contains("was defined as immutable here"));
    }
}
