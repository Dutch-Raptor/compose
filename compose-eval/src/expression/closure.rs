use crate::vm::{FlowEvent, Tracked, TrackedContainer};
use crate::{Eval, Machine};
use compose_library::diag::{IntoSourceDiagnostic, SourceResult, Spanned, bail, error};
use compose_library::{
    Args, Binding, BindingKind, Closure, Func, Library, Scope, Scopes, Value, VariableAccessError,
    World,
};
use compose_syntax::ast::{AstNode, Expr, Ident, Param};
use compose_syntax::{Label, Span, SyntaxNode, ast};
use ecow::{EcoString, EcoVec};
use std::collections::HashMap;

impl Eval for ast::Closure<'_> {
    type Output = Value;

    fn eval(self, vm: &mut Machine) -> SourceResult<Self::Output> {
        let guard = vm.temp_root_guard();

        let mut defaults = Vec::new();
        for param in self.params().children() {
            if let ast::ParamKind::Named(named) = param.kind() {
                defaults.push(named.expr().eval(guard.vm)?);
            }
        }

        let captured = {
            let mut errors = EcoVec::new();
            let mut scope = Scope::new();
            for capture in self.captures().children() {
                let span = capture.binding().span();
                let name = capture.binding().get();
                let binding = match guard.vm.get(&capture.binding()).cloned() {
                    Ok(v) => v,
                    Err(e) => {
                        let VariableAccessError::Unbound(unbound) = e else {
                            errors.push(e.into_source_diagnostic(span));
                            continue;
                        };

                        let mut err = error!(
                            span, "unknown variable `{name}` in closure capture list";
                            label_message: "variable `{name}` is not defined in the outer scope and cannot be captured";
                        );
                        unbound.apply_hint(&mut err);
                        errors.push(err);

                        continue;
                    }
                };

                let value = match validate_capture(capture, &binding, guard.vm) {
                    Ok(v) => v,
                    Err(e) => {
                        errors.extend(e.into_iter());
                        continue;
                    }
                };

                scope.bind(
                    name.clone(),
                    Binding::new(value.clone(), span).with_kind(match capture.is_mut() {
                        true => BindingKind::Mutable,
                        false => BindingKind::Immutable { first_assign: None },
                    }),
                );
            }

            if !errors.is_empty() {
                return Err(errors);
            }

            scope
        };

        let unresolved_captures = {
            let mut visitor = CapturesVisitor::new(
                &guard.vm.frames.top.scopes,
                Some(guard.vm.engine.world.library()),
                &captured,
            );
            visitor.visit(self.body().to_untyped());
            visitor.finish()
        };

        let closure = Closure {
            name: None,
            node: self.to_untyped().clone(),
            defaults,
            num_pos_params: self
                .params()
                .children()
                .filter(|p| matches!(p.kind(), ast::ParamKind::Pos(_)))
                .count(),
            captured,
            unresolved_captures,
        };

        if !guard.vm.context.closure_capture.should_defer() {
            closure.resolve()?
        }

        let param_span = self.params().span();
        Ok(Value::Func(Func::from(closure)).spanned(param_span))
    }
}

fn validate_capture<'a>(
    capture: ast::Capture,
    binding: &'a Binding,
    vm: &mut Machine,
) -> SourceResult<&'a Value> {
    let span = capture.binding().span();
    let name = capture.binding().get();
    if capture.is_mut() && capture.is_ref() && !binding.kind().is_mut() {
        bail!(
            capture.span(), "cannot capture variable `{name}` as `ref mut` because it is not declared as mutable";
            label_message: "capture is declared as a mutable reference";
            label: Label::secondary(binding.span(), "was defined as immutable here");
            note: "captured mutable references must match the mutability of the original declaration";
            hint: "declare the variable as mutable: `let mut {name} = ...`";
            hint: "or remove `mut` from the capture: `|ref {name}, ...|"
        );
    }

    let value = binding.read_checked(span, vm.sink_mut());

    if capture.is_ref() && !value.is_box() {
        bail!(
            span, "cannot capture non reference type by reference";
            label_message: "this captures by reference";
            note: "only boxed values can be captured by reference"
        );
    }

    Ok(value)
}

fn define(
    vm: &mut Machine,
    ident: Ident,
    Spanned { value, span }: Spanned<Value>,
    param: Param,
) -> SourceResult<()> {
    if param.is_ref() && !value.is_box() {
        bail!(
            ident.span(),
            "cannot take a reference to a value type. consider boxing it first with `box::new(value)`"
        );
    }
    if !param.is_ref() && value.is_box() {
        bail!(span, "Cannot bind a box value to a non-reference parameter";
            label_message: "this parameter is a boxed value";
            label: Label::primary(ident.span(), "this parameter is declared as an owned value");
            note: "passing a reference type (like `box`) into an owned parameter is not allowed";
            hint: "if the parameter intends to modify the original value, mark the parameter as `ref mut`";
            hint: "if you want to pass the value by ownership, use `.clone_inner()` to create a new copy";
        );
    }

    let kind = if param.is_mut() {
        BindingKind::ParamMut
    } else {
        BindingKind::Param
    };
    vm.define(ident, value, kind)?;

    Ok(())
}

pub fn eval_closure(closure: &Closure, vm: &mut Machine, args: Args) -> SourceResult<Value> {
    let guard = vm.temp_root_guard();
    let ast_closure = closure
        .node
        .cast::<ast::Closure>()
        .expect("closure is not an ast closure");
    let params = ast_closure.params();
    let body = ast_closure.body();

    // Make sure a gc round is aware that the args are reachable
    guard.vm.track_tmp_root(&args);

    let result = guard
        .vm
        .with_frame(move |vm| {
            let mut args = args;
            if let Some(Spanned { value, span }) = &closure.name {
                vm.try_bind(
                    value.clone(),
                    Binding::new(Func::from(closure.clone()), *span),
                )?;
            }

            for (k, v) in closure.captured.bindings() {
                vm.bind(k.clone(), v.clone());
            }

            let mut defaults = closure.defaults.iter();
            for p in params.children() {
                match p.kind() {
                    ast::ParamKind::Pos(pattern) => match pattern {
                        ast::Pattern::Single(ast::Expr::Ident(ident)) => {
                            define(vm, ident, args.expect(&ident)?, p)?;
                        }
                        pattern => bail!(pattern.span(), "Patterns not supported in closures yet"),
                    },
                    ast::ParamKind::Named(named) => {
                        let name = named.name();
                        let default = defaults.next().unwrap();
                        let value = args
                            .named(&name)?
                            .unwrap_or_else(|| Spanned::new(default.clone(), named.expr().span()));
                        define(vm, name, value, p)?;
                    }
                }
            }

            // Ensure all args have been used
            args.finish()?;

            let output = body.eval(vm)?;
            match &vm.flow {
                None => {}
                Some(FlowEvent::Return(_, Some(explicit))) => return Ok(explicit.clone()),
                Some(FlowEvent::Return(_, None)) => {}
                Some(other) => bail!(other.forbidden()),
            }
            SourceResult::Ok(output)
        })
        .track_tmp_root(guard.vm);

    guard.vm.maybe_gc();

    result
}

/// Visits a closure and determines which variables are captured implicitly.
#[derive(Debug)]
pub struct CapturesVisitor<'a> {
    /// The external scope that variables might be captured from.
    external: &'a Scopes<'a>,
    /// The internal scope of variables defined within the closure.
    internal: Scopes<'a>,
    /// The variables that are captured.
    captures: HashMap<EcoString, Span>,
}

impl<'a> CapturesVisitor<'a> {
    pub fn new(external: &'a Scopes<'a>, library: Option<&'a Library>, existing: &Scope) -> Self {
        let mut inst = Self {
            external,
            internal: Scopes::new(library),
            captures: HashMap::new(),
        };

        for (k, v) in existing.bindings() {
            inst.internal.top.bind(k.clone(), v.clone());
        }

        inst
    }

    pub fn visit(&mut self, node: &'a SyntaxNode) {
        if let Some(ast::Statement::Let(let_binding)) = node.cast() {
            if let Some(init) = let_binding.initial_value() {
                self.visit(init.to_untyped())
            }

            for ident in let_binding.pattern().bindings() {
                self.bind(ident);
            }
            return;
        }

        let expr = match node.cast::<Expr>() {
            Some(expr) => expr,
            None => {
                if let Some(named) = node.cast::<ast::Named>() {
                    // Don't capture the name of a named parameter.
                    self.visit(named.expr().to_untyped());
                    return;
                }

                Expr::default()
            }
        };

        match expr {
            Expr::Ident(ident) => self.capture(ident),
            Expr::CodeBlock(_) => {
                self.internal.enter();
                for child in node.children() {
                    self.visit(child);
                }
                self.internal.exit();
            }
            Expr::FieldAccess(access) => {
                self.visit(access.target().to_untyped());
            }
            Expr::Closure(closure) => {
                for param in closure.params().children() {
                    if let ast::ParamKind::Named(named) = param.kind() {
                        self.visit(named.expr().to_untyped());
                    }
                }

                for capture in closure.captures().children() {
                    self.visit(capture.to_untyped());
                }

                // NOTE: For now we do not try to analyse the body of the closure.
                // This is because the closure might try to recursively call itself
                // and in simple ast walking, that is really hard to resolve correctly.
                // Any errors in the body will be caught when the outer body is evaluated.
            }

            Expr::ForLoop(for_loop) => {
                // Created in outer scope
                self.visit(for_loop.iterable().to_untyped());

                self.internal.enter();
                let pattern = for_loop.binding();
                for ident in pattern.bindings() {
                    self.bind(ident);
                }

                self.visit(for_loop.body().to_untyped());
                self.internal.exit();
            }

            _ => {
                // If not an expression or named, just go over all the children
                for child in node.children() {
                    self.visit(child);
                }
            }
        }
    }

    fn bind(&mut self, ident: Ident) {
        self.internal.top.bind(
            ident.get().clone(),
            Binding::new(Value::unit(), ident.span()),
        );
    }

    fn capture(&mut self, ident: Ident<'a>) {
        if self.internal.get(&ident).is_ok() {
            // Was defined internally, no need to capture
            return;
        }

        // If the value does not exist in the external scope, it is not captured.
        if self.external.get(&ident).is_ok() {
            self.captures
                .entry(ident.get().clone())
                .or_insert(ident.span());
        }
    }

    fn finish(self) -> HashMap<EcoString, Span> {
        self.captures
    }
}

#[cfg(test)]
mod tests {
    use crate::expression::closure::CapturesVisitor;
    use crate::test::*;
    use compose_library::{Scope, Scopes};
    use compose_syntax::{FileId, parse};

    #[test]
    fn capturing() {
        assert_eval(
            r#"
            let a = 10;
            let f = |a| () => a + 1;
            assert::eq(f(), 11);
        "#,
        );

        assert_eval(
            r#"
            let mut a = box::new(5);
            let f = |ref mut a| () => {
                *a += 1;
                *a;
            };
            assert::eq(f(), 6);
        "#,
        );

        assert_eval(
            r#"
            let a = 2;
            let b = 3;
            let f = |a, b| () => a * b;
            assert::eq(f(), 6);
        "#,
        );

        assert_eval(
            r#"
            let mut name = box::new("Alice");
            let age = 30;
            let show = |ref name, age| () => {
                "Name: " + *name + ", Age: " + age.repr();
            };
            assert::eq(show(), "Name: Alice, Age: 30");
            *name = "Bob";
            assert::eq(show(), "Name: Bob, Age: 30");
        "#,
        );
    }

    #[track_caller]
    fn test(scopes: &Scopes, existing_scope: &Scope, text: &str, result: &[&str]) {
        let mut visitor = CapturesVisitor::new(scopes, None, existing_scope);
        let nodes = parse(text, FileId::new("test.comp"));
        for node in &nodes {
            visitor.visit(node);
        }

        let captures = visitor.finish();
        let mut names: Vec<_> = captures.iter().map(|(k, ..)| k).collect();
        names.sort();

        assert_eq!(names, result);
    }

    #[test]
    fn test_captures_visitor() {
        let mut scopes = Scopes::new(None);
        scopes.top.define("f", 0i64);
        scopes.top.define("x", 0i64);
        scopes.top.define("y", 0i64);
        scopes.top.define("z", 0i64);
        let s = &scopes;

        let mut existing = Scope::new();
        existing.define("a", 0i64);
        existing.define("b", 0i64);
        existing.define("c", 0i64);
        let e = &existing;

        // let binding
        test(s, e, "let t = x;", &["x"]);
        test(s, e, "let x = x;", &["x"]);
        test(s, e, "let x;", &[]);
        test(s, e, "let x = 2; x + y;", &["y"]);
        test(s, e, "x + y", &["x", "y"]);

        // assignment
        test(s, e, "x += y;", &["x", "y"]);
        test(s, e, "x = y;", &["x", "y"]);

        // closure definition
        // Closure bodies are ignored
        test(s, e, "let f = () => x + y;", &[]);
        // with capture
        test(s, e, "let f = |x| () => x + y;", &["x"]);
        test(s, e, "let f = |x| () => f();", &["x"]);
        // with params
        test(s, e, "let f = (x, y, z) => f();", &[]);
        // named params
        test(
            s,
            e,
            "let f = (x = x, y = y, z = z) => f();",
            &["x", "y", "z"],
        );

        // for loop
        test(s, e, "for x in y { x + z; };", &["y", "z"]);
        test(s, e, "for x in y { x; }; x", &["x", "y"]);

        // block
        test(s, e, "{ x; };", &["x"]);
        test(s, e, "{ let x; x; };", &[]);
        test(s, e, "{ let x; x; }; x;", &["x"]);

        // field access
        test(s, e, "x.y.f(z);", &["x", "z"]);

        // parenthesized
        test(s, e, "(x + z);", &["x", "z"]);
        test(s, e, "(((x) => x + y) + y);", &["y"]);
    }
}
