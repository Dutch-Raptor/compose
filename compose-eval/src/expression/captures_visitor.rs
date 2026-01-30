use compose_syntax::ast::{Arg, AstNode, Expr, Ident, ParamKind, Statement};
use compose_syntax::{ast, Span};
use ecow::EcoString;
use std::collections::HashMap;
use std::path::PathBuf;
use compose_library::foundations::scope::{Binding, Scope, Scopes};
use compose_library::{Library, Value};

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
        let mut visitor = Self {
            external,
            internal: Scopes::new(library),
            captures: HashMap::new(),
        };

        for (k, v) in existing.bindings() {
            visitor
                .internal
                .top_lexical_mut()
                .bind(k.clone(), v.clone());
        }

        visitor
    }
    pub(crate) fn visit_lambda(&mut self, closure: ast::Lambda<'a>) {
        for param in closure.params().children() {
            match param.kind() {
                ParamKind::Pos(pat) => {
                    for ident in pat.bindings() {
                        self.bind(ident);
                    }
                }
                ParamKind::Named(named) => {
                    self.bind(named.name());
                }
            }
        }

        for capture in closure.captures().children() {
            self.bind(capture.binding());
        }

        for statement in closure.statements() {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: Statement<'a>) {
        self.internal.enter_flow();
        match statement {
            Statement::Let(let_binding) => {
                if let Some(init) = let_binding.initial_value() {
                    self.visit_expr(init)
                }
                for binding in let_binding.pattern().bindings() {
                    self.bind(binding);
                }
            }
            Statement::Expr(expr) => self.visit_expr(expr),
            Statement::Assign(assign) => {
                self.visit_expr(assign.lhs());
                self.visit_expr(assign.rhs());
            }
            Statement::Break(brk) => {
                if let Some(expr) = brk.value() {
                    self.visit_expr(expr);
                }
            }
            Statement::Return(ret) => {
                if let Some(expr) = ret.value() {
                    self.visit_expr(expr);
                }
            }
            Statement::Continue(_) => {}
            Statement::ModuleImport(import) => {
                if let Some(alias) = import.alias() {
                    self.bind(alias);
                } else {
                    let path = PathBuf::from(import.source().as_str());
                    if let Some(stem) = path.file_stem() {
                        self.bind_eco_str(stem.to_string_lossy().into(), import.source_span());
                    }
                }
            }
        }
        self.internal.exit_flow();
    }

    fn visit_expr(&mut self, expr: Expr<'a>) {
        match expr {
            Expr::Ident(ident) => self.capture(ident),
            Expr::CodeBlock(block) => self.visit_code_block(block),
            Expr::FieldAccess(access) => {
                self.visit_expr(access.target());
            }
            Expr::Lambda(closure) => {
                for param in closure.params().children() {
                    if let ParamKind::Named(named) = param.kind() {
                        self.visit_expr(named.expr());
                    }
                }

                for capture in closure.captures().children() {
                    self.capture(capture.binding());
                }

                // NOTE: For now we do not try to analyse the body of the closure.
                // This is because the closure might try to recursively call itself
                // and in simple ast walking, that is really hard to resolve correctly.
                // Any capture errors in the body will be caught when the outer body is evaluated.
            }

            Expr::ForLoop(for_loop) => {
                // Created in outer scope
                self.internal.enter_flow();
                self.visit_expr(for_loop.iterable());

                self.internal.enter_lexical();
                let pattern = for_loop.binding();
                for ident in pattern.bindings() {
                    self.bind(ident);
                }

                for stmt in for_loop.body().statements() {
                    self.visit_statement(stmt);
                }

                self.internal.exit_lexical();
                self.internal.exit_flow();
            }
            Expr::IsExpression(is) => {
                self.visit_expr(is.expr());

                for ident in is.pattern().bindings() {
                    self.bind_flow(ident);
                }
            }
            Expr::Unary(unary) => self.visit_expr(unary.expr()),
            Expr::Unit(_) => {}
            Expr::Binary(binary) => {
                self.visit_expr(binary.lhs());
                self.visit_expr(binary.rhs());
            }
            Expr::Int(_) => {}
            Expr::Str(_) => {}
            Expr::Bool(_) => {}
            Expr::FuncCall(call) => {
                self.visit_expr(call.callee());
                for arg in call.args().items() {
                    match arg {
                        Arg::Pos(expr) => self.visit_expr(expr),
                        Arg::Named(named) => self.visit_expr(named.expr()),
                    }
                }
            }
            Expr::PathAccess(path) => self.visit_expr(path.target()),
            Expr::Parenthesized(paren) => self.visit_expr(paren.expr()),
            Expr::Conditional(cond) => {
                self.internal.enter_flow();
                self.visit_expr(cond.condition().expr());
                self.visit_code_block(cond.consequent());
                self.internal.exit_flow();

                for alternates in cond.cond_alternates() {
                    self.internal.enter_flow();
                    self.visit_expr(alternates.condition().expr());
                    self.visit_code_block(alternates.consequent());
                    self.internal.exit_flow();
                }

                if let Some(else_) = cond.cond_else() {
                    self.visit_code_block(else_.consequent())
                }
            }
            Expr::WhileLoop(while_) => {
                self.internal.enter_flow();
                self.visit_expr(while_.condition().expr());
                self.visit_code_block(while_.body());
                self.internal.exit_flow();
            }
            Expr::Array(arr) => {
                for expr in arr.elements() {
                    self.visit_expr(expr);
                }
            }
            Expr::Range(r) => {
                if let Some(start) = r.start() {
                    self.visit_expr(start);
                }
                if let Some(end) = r.end() {
                    self.visit_expr(end);
                }
            }
            Expr::Map(map) => {
                for entry in map.entries() {
                    match entry.key() {
                        Expr::Ident(_) => {}
                        expr => self.visit_expr(expr),
                    }
                    self.visit_expr(entry.value());
                }
            }
            Expr::IndexAccess(index) => {
                self.visit_expr(index.target());
                self.visit_expr(index.index());
            }
            Expr::MatchExpression(match_) => {
                self.internal.enter_flow();
                self.visit_expr(match_.expr());

                for arm in match_.match_arms() {
                    self.internal.enter_flow();
                    for binding in arm.patterns().flat_map(|pat| pat.bindings()) {
                        self.bind(binding);
                    }
                    if let Some(match_guard) = arm.guard() {
                        self.visit_expr(match_guard);
                    }
                    self.visit_expr(arm.expr());
                    self.internal.exit_flow();
                }

                self.internal.exit_flow();
            }
        }
    }

    fn visit_code_block(&mut self, block: ast::CodeBlock<'a>) {
        self.internal.enter_lexical();
        for child in block.statements() {
            self.visit_statement(child);
        }
        self.internal.exit_lexical();
    }

    fn bind(&mut self, ident: Ident) {
        self.internal.top_lexical_mut().bind(
            ident.get().clone(),
            Binding::new(Value::unit(), ident.span()),
        );
    }

    fn bind_eco_str(&mut self, name: EcoString, span: Span) {
        self.internal
            .top_lexical_mut()
            .bind(name, Binding::new(Value::unit(), span));
    }

    fn bind_flow(&mut self, ident: Ident) {
        if let Some(flow) = self.internal.top_flow_mut() {
            flow.bind(
                ident.get().clone(),
                Binding::new(Value::unit(), ident.span()),
            );
        }
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

    pub(crate) fn finish(self) -> HashMap<EcoString, Span> {
        self.captures
    }
}

#[cfg(test)]
mod tests {
    use crate::expression::captures_visitor::CapturesVisitor;
    use crate::test::{print_diagnostics, TestWorld};
    use compose_library::diag::SourceDiagnostic;
    use compose_library::foundations::scope::{Scope, Scopes};
    use compose_syntax::ast;

    #[track_caller]
    fn test(text: &str, expected_names: &[&str]) {
        let mut scopes = Scopes::new(None);
        scopes.top_lexical_mut().define("f", 0i64);
        scopes.top_lexical_mut().define("x", 0i64);
        scopes.top_lexical_mut().define("y", 0i64);
        scopes.top_lexical_mut().define("z", 0i64);

        let mut existing = Scope::new_lexical();
        existing.define("a", 0i64);
        existing.define("b", 0i64);
        existing.define("c", 0i64);
        let mut visitor = CapturesVisitor::new(&scopes, None, &existing);
        let world = TestWorld::from_str(text);


        let source = world.entrypoint_src();
        let mut fail = false;
        for node in source.nodes() {
            let errors = node
                .errors()
                .into_iter()
                .map(SourceDiagnostic::from)
                .collect::<Vec<_>>();
            if !errors.is_empty() {
                print_diagnostics(&TestWorld::new(), &errors, &[]);
                fail = true;
            }
            visitor.visit_statement(node.cast::<ast::Statement>().expect("expected a statement"));
        }

        if fail {
            panic!("failed to parse");
        }

        let captures = visitor.finish();
        let mut names: Vec<_> = captures.iter().map(|(k, ..)| k).collect();
        names.sort();

        assert_eq!(names, expected_names);
    }

    #[test]
    fn captures_identifier_from_let_initializer() {
        test("let t = x;", &["x"]);
    }

    #[test]
    fn captures_self_reference_in_let_initializer() {
        test("let x = x;", &["x"]);
    }

    #[test]
    fn let_without_initializer_captures_nothing() {
        test("let x;", &[]);
    }

    #[test]
    fn does_not_capture_defined_variables() {
        test("let x = 2; x + y;", &["y"]);
    }

    #[test]
    fn captures_identifiers_from_expression() {
        test("x + y", &["x", "y"]);
    }

    #[test]
    fn assignment_captures_lhs_and_rhs() {
        test("x += y;", &["x", "y"]);
    }

    #[test]
    fn simple_assignment_captures_lhs_and_rhs() {
        test("x = y;", &["x", "y"]);
    }

    #[test]
    fn closure_definition_does_not_capture_from_body() {
        test("let f = { => x + y; }", &[]);
    }

    #[test]
    fn closure_with_capture_list_captures_explicit_names() {
        test("let f = { |x| => x + y; }", &["x"]);
    }

    #[test]
    fn closure_body_referencing_outer_name_does_not_force_capture() {
        test("let f = { |x| => f(); }", &["x"]);
    }

    #[test]
    fn closure_does_not_capture_positional_parameters() {
        test("let f = { x, y, z => f(); }", &[]);
    }

    #[test]
    fn closure_with_named_parameters_captures_default_values() {
        test("let f = { x: x, y: y, z: z => f(); }", &["x", "y", "z"]);
    }

    #[test]
    fn for_loop_captures_iterable_and_body_uses() {
        test("for (x in y) { x + z; };", &["y", "z"]);
    }

    #[test]
    fn for_loop_binding_does_not_escape_and_requires_capture() {
        test("for (x in y) { x; }; x", &["x", "y"]);
    }

    #[test]
    fn block_expression_captures_inner_identifier() {
        test("{ x; };", &["x"]);
    }

    #[test]
    fn block_local_binding_prevents_capture() {
        test("{ let x; x; };", &[]);
    }

    #[test]
    fn block_binding_does_not_escape_and_requires_capture() {
        test("{ let x; x; }; x;", &["x"]);
    }

    #[test]
    fn field_access_captures_receiver_and_arguments() {
        test("x.y.f(z);", &["x", "z"]);
    }

    #[test]
    fn parenthesized_expression_captures_identifiers() {
        test("(x + z);", &["x", "z"]);
    }

    #[test]
    fn nested_parenthesized_closure_does_not_capture_inner_bindings() {
        test("(({ x => x + y }) + y);", &["y"]);
    }

    #[test]
    fn if_flow_binding_does_not_require_capture_of_bound_names() {
        test(
            "if ([1, 2] is [x, y] && y == 2) { x + y + z; }",
            &["z"],
        );
    }

    #[test]
    fn match_arm_binding_does_not_require_capture() {
        test("match (1) { Int x => x + y };", &["y"]);
    }

    #[test]
    fn match_guard_binding_does_not_require_capture() {
        test("match (1) { Int x if x > 0 => y };", &["y"]);
    }

    #[test]
    fn match_guard_flow_binding_does_not_require_capture() {
        test("match (1) { Int x if x is Int y => y };", &[]);
    }

    #[test]
    fn while_flow_binding_does_not_require_capture() {
        test("while (x is [first, ..]) { first }", &["x"]);
    }
}
