pub mod expr_id;
pub mod module;
pub mod scope;
pub mod symbol;

use crate::module::{Module, ModuleId};
use crate::scope::{Frames, Scope, ScopeSource};
use crate::symbol::{SymbolKind, SymbolOrigin, SymbolTable, UnresolvedSymbol};
use compose_library::diag::print_diagnostics;
use compose_library::sink::Sink;
use compose_library::{error, SourceDiagnostic, Value, World};
use compose_syntax::ast::{
    Arg, AstNode, Code, DestructuringItem, Expr, Ident, ParamKind, Pattern, Statement,
};
use compose_syntax::{Label, Span};
use compose_utils::defer;
use compose_utils::id::IdStore;
use ecow::eco_format;
pub use expr_id::{ExprId, ExprIdTable};
use std::ops::DerefMut;
pub use symbol::{Symbol, SymbolId, SymbolUsage};

pub struct NameResolver<'a, 'w> {
    symbol_table: SymbolTable,
    expr_id_table: &'a ExprIdTable,
    world: &'w dyn World,
    frames: Frames,
    symbol_ids: IdStore<SymbolId>,
    modules: Vec<Module>,
    module_ids: IdStore<ModuleId>,
    current_module_idx: usize,
    global: Scope,
    unresolved_symbols: Vec<UnresolvedSymbol>,
    symbol_usage: Vec<SymbolUsage>,
    sink: Sink,
}

impl<'a, 'w> NameResolver<'a, 'w> {
    pub fn new(expr_id_table: &'a ExprIdTable, world: &'w dyn World) -> Self {
        let mut module_ids = IdStore::new();

        let first_module_id = module_ids.next();

        let library = world.library();
        let global_module = Module {
            name: library.global.name().clone(),
            id: first_module_id,
        };

        let module = Module {
            name: "main".into(),
            id: module_ids.next(),
        };

        let mut instance = Self {
            symbol_table: SymbolTable::new(),
            expr_id_table,
            world,
            global: Scope::new_lexical(ScopeSource::module(first_module_id), None),
            frames: Frames::new(),
            symbol_ids: IdStore::new(),
            module_ids,
            unresolved_symbols: vec![],
            current_module_idx: 1,
            modules: vec![global_module, module],
            symbol_usage: vec![],
            sink: Sink::default(),
        };

        // build the global scope
        for (name, binding) in library.global.scope().bindings() {
            let symbol_id = instance.symbol_ids.next();
            let value = binding.read();
            let mut symbol = Symbol::new(
                name.clone(),
                symbol_id,
                first_module_id,
                SymbolOrigin::Implicit,
            );
            symbol.with_kind(match value {
                Value::Bool(_)
                | Value::Unit(_)
                | Value::Str(_)
                | Value::Iterator(_)
                | Value::Array(_)
                | Value::Range(_)
                | Value::Map(_)
                | Value::Box(_)
                | Value::Int(_) => SymbolKind::Static,
                Value::Func(_) => SymbolKind::Function,
                Value::Type(_) => SymbolKind::Type,
                Value::Module(_) => SymbolKind::Module,
            });

            instance.global.bind(name.clone(), symbol_id);
            instance.symbol_table.insert(symbol);
        }

        instance
    }

    fn current_module_id(&self) -> ModuleId {
        self.modules[self.current_module_idx].id
    }

    fn expr_id(&self, span: Span) -> ExprId {
        self.expr_id_table
            .get_expr_id(span)
            .expect("span must exist in exprId table")
    }

    fn bind_ident_lexical(&mut self, ident: Ident<'_>, origin: SymbolOrigin) -> &mut Symbol {
        let name = ident.get();

        let symbol = Symbol::new(
            name.clone(),
            self.symbol_ids.next(),
            self.current_module_id(),
            origin,
        );
        self.bind_symbol_lexical(symbol)
    }

    fn bind_symbol_lexical(&mut self, symbol: Symbol) -> &mut Symbol {
        self.frames
            .bind_lexical(symbol.name.clone(), symbol.symbol_id);
        self.symbol_table.insert(symbol)
    }

    fn get(&self, name: impl AsRef<str>) -> Option<SymbolId> {
        let name = name.as_ref();
        if let Some(id) = self.frames.get(name) {
            return Some(id);
        }

        if let Some(id) = self.global.get(name) {
            return Some(id);
        }

        None
    }

    fn symbol(&self, id: SymbolId) -> &Symbol {
        self.symbol_table.get(id).expect("symbol must exist")
    }

    fn bind_ident_flow(&mut self, ident: Ident<'_>) -> &mut Symbol {
        let expr_id = self
            .expr_id_table
            .get_expr_id(ident.span())
            .expect("span to exist in exprId table");
        let name = ident.get();

        let symbol = Symbol::new(
            name.clone(),
            self.symbol_ids.next(),
            self.current_module_id(),
            SymbolOrigin::Flow(expr_id),
        );

        self.bind_symbol_flow(symbol)
    }

    fn bind_symbol_flow(&mut self, symbol: Symbol) -> &mut Symbol {
        self.frames.bind_flow(symbol.name.clone(), symbol.symbol_id);

        self.symbol_table.insert(symbol)
    }

    fn record_usage(&mut self, ident: Ident<'_>) -> Result<&Symbol, &mut UnresolvedSymbol> {
        let name = ident.get();

        let expr_id = self
            .expr_id_table
            .get_expr_id(ident.span())
            .expect("span to exist in exprId table");

        match self.get(name) {
            Some(id) => {
                self.symbol_usage.push(SymbolUsage {
                    symbol_id: id,
                    expr_id,
                });
                Ok(self.symbol(id))
            }
            None => {
                self.unresolved_symbols.push(UnresolvedSymbol::new(
                    name.clone(),
                    expr_id,
                    self.frames.current().source().clone(),
                ));
                Err(self
                    .unresolved_symbols
                    .last_mut()
                    .expect("was just inserted"))
            }
        }
    }

    pub fn resolve(&mut self) {
        let source = self
            .world
            .source(self.world.entry_point())
            .expect("entry point must exist");
        let code = source
            .root_node()
            .cast::<Code<'_>>()
            .expect("Root node must be a code node");

        self.frames
            .enter_frame(ScopeSource::module(self.current_module_id()));

        for stmt in code.statements() {
            self.resolve_statement(stmt);
        }

        self.frames.exit_frame();

        let mut sorted_unresolved = self.unresolved_symbols.clone();
        sorted_unresolved.sort_by(|a, b| a.expr_id.cmp(&b.expr_id));
        for unresolved in self.unresolved_symbols.iter() {
            let span = self
                .expr_id_table
                .get_span(unresolved.expr_id)
                .expect("span to exist");
            let name = unresolved.name.clone();

            let diag = SourceDiagnostic::error(span, eco_format!("unresolved symbol `{name}`"));
            print_diagnostics(self.world, &[diag], &[], false)
                .expect("diagnostics must be printed");
        }

        print_diagnostics(self.world, &self.sink.errors, &self.sink.warnings, false)
            .expect("diagnostics must be printed");

        let mut symbols = self.symbol_table.iter().collect::<Vec<_>>();
        symbols.sort_by_key(|(id, _)| *id);

        for (id, symbol) in symbols {
            let usages = self
                .symbol_usage
                .iter()
                .filter(|u| u.symbol_id == *id)
                .collect::<Vec<_>>();

            if matches!(symbol.symbol_origin, SymbolOrigin::Implicit) && usages.is_empty() {
                // Don't print diagnostics for implicit symbols that are not used.
                continue;
            }

            let span = symbol
                .symbol_origin
                .expr_id()
                .and_then(|id| self.expr_id_table.get_span(id))
                .unwrap_or(Span::detached());
            let name = symbol.name.clone();
            let origin = symbol.symbol_origin;

            let mut diag = SourceDiagnostic::create_note(
                span,
                eco_format!("{id:?}: `{name}`, kind: {origin:?}"),
            )
            .with_label_message("defined here");
            diag.labels.extend(usages.iter().map(|u| {
                Label::secondary(
                    self.expr_id_table
                        .get_span(u.expr_id)
                        .expect("span to exist"),
                    "usage",
                )
            }));

            print_diagnostics(self.world, &[diag], &[], false)
                .expect("diagnostics must be printed");
        }
    }

    fn in_flow_guard(&mut self, source: ScopeSource) -> impl DerefMut<Target = Self> {
        let open_flow = !self.frames.in_flow();
        if open_flow {
            self.frames.enter_flow(source);
        }

        defer(self, move |this| {
            if open_flow {
                this.frames.exit_flow();
            }
        })
    }

    fn new_flow_guard(&mut self, source: ScopeSource) -> impl DerefMut<Target = Self> {
        self.frames.enter_flow(source);
        defer(self, |this| {
            this.frames.exit_flow();
        })
    }

    fn new_lexical_guard(&mut self, source: ScopeSource) -> impl DerefMut<Target = Self> {
        self.frames.enter_lexical(source);
        defer(self, |this| {
            this.frames.exit_lexical();
        })
    }

    fn resolve_type(&mut self, ident: Ident<'_>) {
        let Ok(symbol) = self.record_usage(ident) else {
            return;
        };

        if let SymbolKind::Type = symbol.symbol_kind {
            return;
        }

        let name = symbol.name.clone();
        let origin_expr_id = symbol.symbol_origin.expr_id();
        let origin_kind = symbol.symbol_kind;
        let origin_span = origin_expr_id
            .and_then(|id| self.expr_id_table.get_span(id))
            .unwrap_or(Span::detached());

        let usage_span = ident.span();

        self.sink.error(error!(
            usage_span,
            "expected a type, but found {origin_kind} `{name}`";
            label_message: "`{name}` is used as a type here";
            label: Label::secondary(origin_span, eco_format!("`{name}` was defined here as a {origin_kind}"))
        ))
    }

    pub fn resolve_pattern(&mut self, pat: Pattern<'_>, bind: &mut impl Fn(&mut Self, Ident<'_>)) {
        match pat {
            Pattern::Single(e) => {
                if let Expr::Ident(ident) = e {
                    bind(self, ident);
                }
            }
            Pattern::PlaceHolder(_) => { /* placeholder has no symbols */ }
            Pattern::Destructuring(destruct) => {
                for item in destruct.items() {
                    match item {
                        DestructuringItem::Pattern(p) => self.resolve_pattern(p, bind),
                        DestructuringItem::Named(named) => {
                            self.resolve_pattern(named.pattern(), bind);
                        }
                        DestructuringItem::Spread(s) => {
                            if let Some(ident) = s.sink_ident() {
                                bind(self, ident);
                            }
                        }
                    }
                }
            }
            Pattern::LiteralPattern(_) => {}
            Pattern::TypedPattern(typed) => {
                self.resolve_type(typed.ty());
                self.resolve_pattern(typed.pattern(), bind);
            }
        }
    }

    pub fn resolve_expression(&mut self, expression: Expr<'_>) {
        match expression {
            Expr::Unary(un) => self.resolve_expression(un.expr()),
            Expr::Unit(_) => {}
            Expr::Ident(ident) => {
                let _ = self.record_usage(ident);
            }
            Expr::Binary(bin) => {
                // if not already in a flow scope, enter one now
                let mut self_ = self.in_flow_guard(ScopeSource::expr(
                    self.current_module_id(),
                    self.expr_id_table.get_expr_id(bin.span()).unwrap(),
                ));

                self_.resolve_expression(bin.lhs());
                self_.resolve_expression(bin.rhs());
            }
            Expr::Int(_) => {}
            Expr::CodeBlock(code) => {
                let mut self_ = self.new_lexical_guard(ScopeSource::expr(
                    self.current_module_id(),
                    self.expr_id_table.get_expr_id(code.span()).unwrap(),
                ));
                for stmt in code.statements() {
                    self_.resolve_statement(stmt);
                }
            }
            Expr::Str(_) => {}
            Expr::Bool(_) => {}
            Expr::FuncCall(call) => {
                for arg in call.args().items() {
                    match arg {
                        Arg::Pos(expr) => self.resolve_expression(expr),
                        Arg::Named(named) => self.resolve_expression(named.expr()),
                    }
                }

                self.resolve_expression(call.callee());
            }
            Expr::FieldAccess(access) => self.resolve_expression(access.target()),
            Expr::PathAccess(_) => {
                // TODO: This will require doing module resolution and stuff
                unimplemented!("implement module resolution")
            }
            Expr::Parenthesized(par) => self.resolve_expression(par.expr()),
            Expr::Conditional(cond) => {
                {
                    let mut self_ = self.new_flow_guard(ScopeSource::expr(
                        self.current_module_id(),
                        self.expr_id_table
                            .get_expr_id(cond.condition().span())
                            .unwrap(),
                    ));
                    self_.resolve_expression(cond.condition().expr());
                    let scope_source = ScopeSource::expr(
                        self_.current_module_id(),
                        self_.expr_id(cond.consequent().span()),
                    );
                    let mut self_ = self_.new_lexical_guard(scope_source);
                    self_.resolve_expression(
                        cond.consequent()
                            .cast()
                            .expect("Code block is an expression"),
                    );
                }

                for alternate in cond.cond_alternates() {
                    let mut self_ = self.new_flow_guard(ScopeSource::expr(
                        self.current_module_id(),
                        self.expr_id_table
                            .get_expr_id(alternate.condition().span())
                            .unwrap(),
                    ));
                    self_.resolve_expression(alternate.condition().expr());
                    let scope_source = ScopeSource::expr(
                        self_.current_module_id(),
                        self_.expr_id(alternate.consequent().span()),
                    );
                    let mut self_ = self_.new_lexical_guard(scope_source);
                    self_.resolve_expression(
                        alternate
                            .consequent()
                            .cast()
                            .expect("Code block is an expression"),
                    );
                }

                if let Some(cond_else) = cond.cond_else() {
                    let scope_source = ScopeSource::expr(
                        self.current_module_id(),
                        self.expr_id(cond_else.consequent().span()),
                    );
                    let mut self_ = self.new_lexical_guard(scope_source);
                    self_.resolve_expression(
                        cond_else
                            .consequent()
                            .cast()
                            .expect("Code block is an expression"),
                    );
                }
            }
            Expr::WhileLoop(while_) => {
                let source = ScopeSource::expr(
                    self.current_module_id(),
                    self.expr_id_table.get_expr_id(while_.span()).unwrap(),
                );
                let mut self_ = self.new_flow_guard(source);

                self_.resolve_expression(while_.condition().expr());

                let source = ScopeSource::expr(
                    self_.current_module_id(),
                    self_
                        .expr_id_table
                        .get_expr_id(while_.body().span())
                        .unwrap(),
                );
                let mut self_ = self_.new_lexical_guard(source);

                self_
                    .resolve_expression(while_.body().cast().expect("Code block is an expression"));
            }
            Expr::ForLoop(for_) => {
                let source = ScopeSource::expr(
                    self.current_module_id(),
                    self.expr_id_table.get_expr_id(for_.span()).unwrap(),
                );
                let mut self_ = self.new_flow_guard(source);

                self_.resolve_expression(for_.iterable());

                let source = ScopeSource::expr(
                    self_.current_module_id(),
                    self_.expr_id_table.get_expr_id(for_.body().span()).unwrap(),
                );

                let mut self_ = self_.new_lexical_guard(source);

                self_.resolve_pattern(for_.binding(), &mut |this, ident| {
                    let expr_id = this.expr_id(ident.span());
                    this.bind_ident_lexical(ident, SymbolOrigin::Local(expr_id));
                });

                self_.resolve_expression(for_.body().cast().expect("Code block is an expression"));
            }
            Expr::Array(arr) => {
                for item in arr.elements() {
                    self.resolve_expression(item);
                }
            }
            Expr::Range(range) => {
                if let Some(start) = range.start() {
                    self.resolve_expression(start);
                }
                if let Some(end) = range.end() {
                    self.resolve_expression(end);
                }
            }
            Expr::Map(map) => {
                for entry in map.entries() {
                    match entry.key() {
                        Expr::Ident(_) => {}
                        expr => self.resolve_expression(expr),
                    }
                    self.resolve_expression(entry.value());
                }
            }
            Expr::Lambda(lambda) => {
                // first evaluate default params
                for param in lambda.params().children() {
                    match param.kind() {
                        ParamKind::Pos(_) => {
                            // bind these after named params have been evaluated
                        }
                        ParamKind::Named(named) => {
                            self.resolve_expression(named.expr());
                        }
                    }
                }

                // evaluate captured variables
                for capture in lambda.captures().children() {
                    let _ = self.record_usage(capture.binding());
                }
                let source = ScopeSource::expr(
                    self.current_module_id(),
                    self.expr_id_table.get_expr_id(lambda.span()).unwrap(),
                );
                let mut self_ = self.new_lexical_guard(source);
                // introduce the captures as bindings before params
                for capture in lambda.captures().children() {
                    let Some(captured) = self_.get(capture.binding().get()) else {
                        // adding an unresolved symbol should have already been handled
                        continue;
                    };
                    let captured_symbol =
                        self_.symbol_table.get(captured).expect("symbol must exist");

                    if !captured_symbol.symbol_origin.should_capture() {
                        let origin = captured_symbol.symbol_origin;
                        self_.sink.error(error!(
                            capture.span(),
                            "lambdas can only capture local variables and parameters";
                            label_message: "`{}` is not a capturable item as it is {} {}",
                            capture.binding().get(),
                            origin.article(),
                            origin.noun(),
                        ));
                        continue;
                    }

                    let origin = SymbolOrigin::Capture {
                        source: captured_symbol.symbol_id,
                        capture_decl: self_.expr_id_table.get_expr_id(capture.span()).unwrap(),
                    };
                    self_.bind_ident_lexical(capture.binding(), origin);
                }
                for param in lambda.params().children() {
                    match param.kind() {
                        ParamKind::Pos(pos) => {
                            self_.resolve_pattern(pos, &mut |this, ident| {
                                let expr_id = this.expr_id(ident.span());
                                this.bind_ident_lexical(ident, SymbolOrigin::Param(expr_id));
                            });
                        }
                        ParamKind::Named(named) => {
                            let origin = SymbolOrigin::Param(
                                self_
                                    .expr_id_table
                                    .get_expr_id(named.name().span())
                                    .unwrap(),
                            );
                            self_.bind_ident_lexical(named.name(), origin);
                        }
                    }
                }

                for stmt in lambda.statements() {
                    self_.resolve_statement(stmt);
                }
            }
            Expr::IndexAccess(idx) => {
                self.resolve_expression(idx.target());
                self.resolve_expression(idx.index());
            }
            Expr::MatchExpression(mat) => {
                let current_module = self.current_module_id();
                let mut self_ = self.new_flow_guard(ScopeSource::expr(
                    current_module,
                    self.expr_id_table.get_expr_id(mat.span()).unwrap(),
                ));

                // The expression matched on might introduce flow bindings
                // `match (v is [a, b]) { true if a > 2 => { ... }, ... }
                self_.resolve_expression(mat.expr());

                for arm in mat.match_arms() {
                    let source = ScopeSource::expr(
                        self_.current_module_id(),
                        self_.expr_id_table.get_expr_id(arm.span()).unwrap(),
                    );

                    let mut self_ = self_.new_flow_guard(source);
                    for pat in arm.patterns() {
                        self_.resolve_pattern(pat, &mut |this, ident| {
                            this.bind_ident_flow(ident);
                        });
                    }

                    if let Some(guard) = arm.guard() {
                        self_.resolve_expression(guard);
                    }

                    let source = ScopeSource::expr(
                        self_.current_module_id(),
                        self_.expr_id_table.get_expr_id(arm.expr().span()).unwrap(),
                    );
                    let mut self_ = self_.new_lexical_guard(source);
                    self_.resolve_expression(arm.expr());
                }
            }
            Expr::IsExpression(is) => {
                self.resolve_expression(is.expr());
                self.resolve_pattern(is.pattern(), &mut |this, ident| {
                    this.bind_ident_flow(ident);
                });
            }
        }
    }

    pub fn resolve_statement(&mut self, statement: Statement<'_>) {
        match statement {
            Statement::Expr(expr) => self.resolve_expression(expr),
            Statement::Let(let_binding) => {
                if let Some(init) = let_binding.initial_value() {
                    self.resolve_expression(init);
                }

                let bindings = let_binding.pattern().bindings();
                for binding in bindings {
                    self.bind_ident_lexical(
                        binding,
                        SymbolOrigin::Local(self.expr_id(binding.span())),
                    );
                }
            }
            Statement::Assign(assign) => {
                self.resolve_expression(assign.lhs());
                self.resolve_expression(assign.rhs());
            }
            Statement::Break(_) => {}
            Statement::Return(_) => {}
            Statement::Continue(_) => {}
            Statement::ModuleImport(_) => {}
        }
    }
}
