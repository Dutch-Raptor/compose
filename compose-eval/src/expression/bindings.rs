use crate::expression::pattern;
use crate::expression::pattern::{PatternContext, PatternMatchResult};
use crate::vm::{ErrorMode, Machine};
use crate::Eval;
use compose_library::diag::{bail, At, SourceResult};
use compose_library::foundations::scope::{BindingKind, Visibility};
use compose_syntax::ast;
use compose_syntax::ast::AstNode;
use crate::evaluated::Evaluated;

impl Eval for ast::Ident<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let span = self.span();

        let binding = vm.frames.top.scopes.get(&self).at(span)?;

        let mutable = binding.is_mutable();

        Ok(Evaluated::new(
            binding.read_checked(span, &mut vm.engine.sink).clone(),
            mutable,
        )
        .with_origin(binding.span()))
    }
}

impl<'a> Eval for ast::LetBinding<'a> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let has_init = self.has_initial_value();
        let init = self.initial_value();

        let binding_kind = match (self.is_mut(), has_init) {
            (true, true) => BindingKind::Mutable,
            (false, true) => BindingKind::Immutable { first_assign: None },
            (true, false) => BindingKind::UninitializedMutable,
            (false, false) => BindingKind::Uninitialized,
        };

        let visibility = if self.is_public() {
            Visibility::Public
        } else {
            Visibility::Private
        };

        let value = match init {
            Some(expr) => {
                vm.with_closure_capture_errors_mode(ErrorMode::Deferred, |vm| expr.eval(vm))?
            }
            None => Evaluated::unit(),
        };

        // handle control flow
        if vm.flow.is_some() {
            return Ok(Evaluated::unit());
        }

        let matched = pattern::destructure_pattern(
            vm,
            self.pattern(),
            value.value,
            PatternContext::LetBinding,
            binding_kind,
            visibility,
        )?;

        match matched {
            PatternMatchResult::NotMatched(err) => bail!(err),
            PatternMatchResult::Matched => Ok(Evaluated::unit()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test::{assert_eval, assert_eval_with_vm, eval_code_with_vm, TestWorld};
    use compose_error_codes::{E0004_MUTATE_IMMUTABLE_VARIABLE, W0001_USED_UNINITIALIZED_VARIABLE};
    use compose_library::foundations::scope::BindingKind;
    use compose_library::foundations::types::UnitValue;
    use compose_library::Value;
    use crate::Machine;

    #[test]
    fn test_let_binding() {
        let world = TestWorld::new();
        let mut vm = Machine::new(&world);
        eval_code_with_vm(&mut vm, &world, "let a = 3")
            .value
            .expect("failed to evaluate");

        let binding = vm.get("a").unwrap();

        assert_eq!(binding.read(), &Value::Int(3));
        assert_eq!(
            binding.kind(),
            BindingKind::Immutable { first_assign: None }
        );
    }

    #[test]
    fn test_let_mut_binding() {
        let world = TestWorld::new();
        let mut vm = Machine::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let mut a = 3");

        let binding = vm.get("a").unwrap();

        assert_eq!(binding.read(), &Value::Int(3));
        assert_eq!(binding.kind(), BindingKind::Mutable);
    }

    #[test]
    fn test_let_binding_without_value() {
        let world = TestWorld::new();
        let mut vm = Machine::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let a");

        let binding = vm.get("a").unwrap();

        assert_eq!(binding.read(), &Value::unit());
        assert_eq!(binding.kind(), BindingKind::Uninitialized);
    }

    #[test]
    fn test_let_mut_binding_without_value() {
        let world = TestWorld::new();
        let mut vm = Machine::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let mut a");

        let binding = vm.get("a").unwrap();
        assert_eq!(binding.read(), &Value::unit());
        assert_eq!(binding.kind(), BindingKind::UninitializedMutable);
    }

    #[test]
    fn test_read_ident() {
        let world = TestWorld::new();
        let mut vm = Machine::new(&world);
        // define the variable
        assert_eval_with_vm(&mut vm, &world, "let a = 3");
        // read the variable
        let result = assert_eval_with_vm(&mut vm, &world, "a");
        assert_eq!(result, Value::Int(3));
    }

    #[test]
    fn test_read_uninitialised_variable() {
        let world = TestWorld::new();
        let mut vm = Machine::new(&world);
        // set up the variable
        assert_eval_with_vm(&mut vm, &world, "let a");
        // reading emits warning
        let result = eval_code_with_vm(&mut vm, &world, "a")
            .assert_warnings(&[&W0001_USED_UNINITIALIZED_VARIABLE])
            .assert_no_errors()
            .get_value();

        assert_eq!(result, Value::unit());
    }

    #[test]
    fn integration() {
        let result = assert_eval(
            r#"
            let a = 3;
            let b = 4;
            let c = a + b;
            c * 2;
        "#,
        );

        assert_eq!(result, Value::Int(14));
    }

    #[test]
    fn assign_mut() {
        let world = TestWorld::new();
        let mut vm = Machine::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let mut a = 3");

        let binding = vm.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Mutable);

        assert_eval_with_vm(&mut vm, &world, "a = 4");
        let result = assert_eval_with_vm(&mut vm, &world, "a");
        assert_eq!(result, Value::Int(4));

        // should still be mutable
        let binding = vm.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Mutable);
    }

    #[test]
    fn assign_mut_uninitialised() {
        let world = TestWorld::new();
        let mut vm = Machine::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let mut a");

        let binding = vm.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::UninitializedMutable);
        assert_eq!(binding.read(), &Value::Unit(UnitValue));

        assert_eval_with_vm(&mut vm, &world, "a = 4");
        let result = assert_eval_with_vm(&mut vm, &world, "a");
        assert_eq!(result, Value::Int(4));

        // should now be mutable
        let binding = vm.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Mutable);
    }

    #[test]
    fn assign_uninitialised() {
        let world = TestWorld::new();
        let mut vm = Machine::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let a");

        let binding = vm.get("a").unwrap();
        assert_eq!(binding.kind(), BindingKind::Uninitialized);
        assert_eq!(binding.read(), &Value::unit());

        assert_eval_with_vm(&mut vm, &world, "a = 4");
        let result = assert_eval_with_vm(&mut vm, &world, "a");
        assert_eq!(result, Value::Int(4));

        // should now be immutable
        let binding = vm.get("a").unwrap();
        assert!(matches!(binding.kind(), BindingKind::Immutable { .. }));
    }

    #[test]
    fn assign_immut_error() {
        let world = TestWorld::new();
        let mut vm = Machine::new(&world);
        assert_eval_with_vm(&mut vm, &world, "let a = 3");
        let binding = vm.get("a").unwrap();
        assert_eq!(
            binding.kind(),
            BindingKind::Immutable { first_assign: None }
        );
        assert_eq!(binding.read(), &Value::Int(3));

        eval_code_with_vm(&mut vm, &world, "a = 4")
            .assert_errors(&[&E0004_MUTATE_IMMUTABLE_VARIABLE]);
    }
}
