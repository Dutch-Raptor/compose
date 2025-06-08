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
            .read_checked(span, &mut vm.engine.sink)
            .clone())
    }
}

impl<'a> Eval for ast::LetBinding<'a> {
    type Output = Value;

    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output> {
        let has_init = self.has_initial_value();
        let init = self.initial_value();

        let binding_kind = match (self.is_mut(), has_init) {
            (true, true) => compose_library::BindingKind::Mutable,
            (false, true) => compose_library::BindingKind::Variable { first_assign: None },
            (true, false) => compose_library::BindingKind::UninitializedMutable,
            (false, false) => compose_library::BindingKind::Uninitialized,
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

pub fn destructure_pattern(
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
        Pattern::Destructuring(_) => {
            unimplemented!("destructuring")
        }
    };

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::{assert_eval, assert_eval_with_vm, eval_code_with_vm, TestWorld};
    use compose_error_codes::{E0004_MUTATE_IMMUTABLE_VARIABLE, W0001_USED_UNINITIALIZED_VARIABLE};
    use compose_library::{BindingKind, UnitValue};

    #[test]
    fn test_let_binding() {
        let world = TestWorld::new();
        let mut vm = Vm::new(&world);
        eval_code_with_vm(&mut vm, &world, "let a = 3")
            .value
            .expect("failed to evaluate");

        let binding = vm.scopes.top.get("a").unwrap();

        assert_eq!(binding.read(), &Value::Int(3));
        assert_eq!(binding.kind(), BindingKind::Variable { first_assign: None });
    }

    #[test]
    fn test_let_mut_binding() {
        let world = TestWorld::new();
        let mut vm = Vm::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let mut a = 3");

        let binding = vm.scopes.top.get("a").unwrap();

        assert_eq!(binding.read(), &Value::Int(3));
        assert_eq!(binding.kind(), BindingKind::Mutable);
    }

    #[test]
    fn test_let_binding_without_value() {
        let world = TestWorld::new();
        let mut vm = Vm::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let a");

        let binding = vm.scopes.top.get("a").unwrap();

        assert_eq!(binding.read(), &Value::unit());
        assert_eq!(binding.kind(), BindingKind::Uninitialized);
    }

    #[test]
    fn test_let_mut_binding_without_value() {
        let world = TestWorld::new();
        let mut vm = Vm::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let mut a");

        let binding = vm.scopes.top.get("a").unwrap();
        assert_eq!(binding.read(), &Value::unit());
        assert_eq!(binding.kind(), BindingKind::UninitializedMutable);
    }

    #[test]
    fn test_read_ident() {
        let world = TestWorld::new();
        let mut vm = Vm::new(&world);
        // define the variable
        assert_eval_with_vm(&mut vm, &world, "let a = 3");
        // read the variable
        let result = assert_eval_with_vm(&mut vm, &world, "a");
        assert_eq!(result, Value::Int(3));
    }

    #[test]
    fn test_read_uninitialised_variable() {
        let world = TestWorld::new();
        let mut vm = Vm::new(&world);
        // set up the variable
        assert_eval_with_vm(&mut vm, &world, "let a");
        // reading emits warning
        let result = eval_code_with_vm(&mut vm, &world, "a")
            .assert_warnings(&[W0001_USED_UNINITIALIZED_VARIABLE])
            .assert_no_errors()
            .get_value();

        assert_eq!(result, Value::unit());
    }

    #[test]
    fn integration() {
        let result = assert_eval(
            r#"
            let a = 3
            let b = 4
            let c = a + b
            c * 2
        "#,
        );

        assert_eq!(result, Value::Int(14));
    }

    #[test]
    fn assign_mut() {
        let world = TestWorld::new();
        let mut vm = Vm::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let mut a = 3");

        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Mutable);

        assert_eval_with_vm(&mut vm, &world, "a = 4");
        let result = assert_eval_with_vm(&mut vm, &world, "a");
        assert_eq!(result, Value::Int(4));

        // should still be mutable
        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Mutable);
    }

    #[test]
    fn assign_mut_uninitialised() {
        let world = TestWorld::new();
        let mut vm = Vm::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let mut a");

        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::UninitializedMutable);
        assert_eq!(binding.read(), &Value::Unit(UnitValue));

        assert_eval_with_vm(&mut vm, &world, "a = 4");
        let result = assert_eval_with_vm(&mut vm, &world, "a");
        assert_eq!(result, Value::Int(4));

        // should now be mutable
        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Mutable);
    }

    #[test]
    fn assign_uninitialised() {
        let world = TestWorld::new();
        let mut vm = Vm::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let a");

        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Uninitialized);
        assert_eq!(binding.read(), &Value::unit());

        assert_eval_with_vm(&mut vm, &world, "a = 4");
        let result = assert_eval_with_vm(&mut vm, &world, "a");
        assert_eq!(result, Value::Int(4));

        // should now be immutable
        let binding = vm.scopes.get("a").unwrap();
        assert!(matches!(binding.kind(), BindingKind::Variable { .. }));
    }

    #[test]
    fn assign_immut_error() {
        let world = TestWorld::new();
        let mut vm = Vm::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let a = 3");
        let binding = vm.scopes.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Variable { first_assign: None });
        assert_eq!(binding.read(), &Value::Int(3));

        eval_code_with_vm(&mut vm, &world, "a = 4")
            .assert_errors(&[E0004_MUTATE_IMMUTABLE_VARIABLE]);
    }
}
