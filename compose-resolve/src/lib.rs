pub mod expr_id;
pub mod module;
pub mod scope;
pub mod symbol;

use crate::module::{Module, ModuleId};
use crate::scope::{Frames, Scope, ScopeId, ScopeSource};
use crate::symbol::{SymbolKind, SymbolOrigin, SymbolTable, UnresolvedSymbol};
use compose_library::diag::print_diagnostics;
use compose_library::sink::Sink;
use compose_library::{SourceDiagnostic, Value, World, error};
use compose_syntax::ast::ty::Type;
use compose_syntax::ast::{
    Arg, AstNode, Code, DestructuringItem, Expr, FnItem, Ident, ParamKind, Pattern, Statement,
};
use compose_syntax::{Label, Span};
pub use compose_typeinfo::SymbolId;
use compose_utils::id::IdStore;
use compose_utils::{defer, trace_fn, trace_log};
use ecow::eco_format;
pub use expr_id::{ExprId, ExprIdTable};
use fxhash::FxHashMap;
use std::ops::DerefMut;
pub use symbol::{Symbol, SymbolUsage};

pub struct ResolutionResult {
    pub modules: Vec<Module>,
    pub symbol_table: SymbolTable,
    pub sink: Sink,
    pub usages: Vec<SymbolUsage>,
    pub unresolved_symbols: Vec<UnresolvedSymbol>,
    pub scopes: Vec<Scope>,
    pub expr_to_symbol: FxHashMap<ExprId, SymbolId>,
}

pub struct NameResolver<'a, 'w> {
    symbol_table: SymbolTable,
    expr_id_table: &'a ExprIdTable,
    world: &'w dyn World,
    frames: Frames,
    symbol_ids: IdStore<SymbolId>,
    expr_to_symbol: FxHashMap<ExprId, SymbolId>,
    modules: Vec<Module>,
    module_ids: IdStore<ModuleId>,
    scope_ids: IdStore<ScopeId>,
    current_module_idx: usize,
    global: Scope,
    unresolved_symbols: Vec<UnresolvedSymbol>,
    symbol_usage: Vec<SymbolUsage>,
    sink: Sink,
    emit_diagnostics: bool,
}

impl<'a, 'w> NameResolver<'a, 'w> {
    pub fn new(expr_id_table: &'a ExprIdTable, world: &'w dyn World) -> Self {
        let mut module_ids = IdStore::new();
        let mut scope_ids = IdStore::new();

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
            global: Scope::new_lexical(
                scope_ids.next(),
                ScopeSource::module(first_module_id),
                None,
            ),
            scope_ids,
            frames: Frames::new(),
            symbol_ids: IdStore::new(),
            module_ids,
            unresolved_symbols: vec![],
            current_module_idx: 1,
            modules: vec![global_module, module],
            symbol_usage: vec![],
            sink: Sink::default(),
            expr_to_symbol: FxHashMap::default(),
            emit_diagnostics: true,
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

    pub fn set_emit_diagnostics(&mut self, emit_diagnostics: bool) {
        self.emit_diagnostics = emit_diagnostics;
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
        trace_log!("binding symbol {symbol:?} in lexical scope");

        if let Some(expr_id) = symbol.symbol_origin.expr_id() {
            self.expr_to_symbol.insert(expr_id, symbol.symbol_id);
        }

        self.frames
            .bind_lexical(symbol.name.clone(), symbol.symbol_id);
        self.symbol_table.insert(symbol)
    }

    fn get(&self, name: impl AsRef<str>) -> Option<SymbolId> {
        let name = name.as_ref();

        self.frames.get(name).or_else(|| self.global.get(name))
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
        trace_log!("binding symbol {symbol:?} in flow");

        if let Some(expr_id) = symbol.symbol_origin.expr_id() {
            self.expr_to_symbol.insert(expr_id, symbol.symbol_id);
        }

        self.frames
            .bind_flow(symbol.name.clone(), symbol.symbol_id)
            .expect("must be in flow scope when calling bind_symbol_flow");

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
                self.expr_to_symbol.insert(expr_id, id);
                trace_log!("usage of symbol {id:?} {name} in {expr_id:?}");
                Ok(self.symbol(id))
            }
            None => {
                self.unresolved_symbols.push(UnresolvedSymbol::new(
                    name.clone(),
                    expr_id,
                    self.frames.current().source().clone(),
                ));
                trace_log!("unresolved symbol {name} in {expr_id:?}");
                Err(self
                    .unresolved_symbols
                    .last_mut()
                    .expect("was just inserted"))
            }
        }
    }

    pub fn resolve(&mut self) -> ResolutionResult {
        let source = self
            .world
            .source(self.world.entry_point())
            .expect("entry point must exist");
        let code = source
            .root_node()
            .cast::<Code<'_>>()
            .expect("Root node must be a code node");

        self.frames.enter_frame(
            self.scope_ids.next(),
            ScopeSource::module(self.current_module_id()),
        );

        self.resolve_code(code.items().collect(), code.statements().collect());

        self.frames.exit_frame();

        let mut sorted_unresolved = self.unresolved_symbols.clone();
        sorted_unresolved.sort_by(|a, b| a.expr_id.cmp(&b.expr_id));
        if self.emit_diagnostics {
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
        }

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

            if self.emit_diagnostics {
                print_diagnostics(self.world, &[diag], &[], false)
                    .expect("diagnostics must be printed");
            }
        }

        ResolutionResult {
            modules: self.modules.clone(),
            symbol_table: self.symbol_table.clone(),
            sink: self.sink.clone(),
            usages: self.symbol_usage.clone(),
            unresolved_symbols: self.unresolved_symbols.clone(),
            scopes: self.frames.to_scopes(),
            expr_to_symbol: self.expr_to_symbol.clone(),
        }
    }

    fn in_flow_guard(&mut self, source: ScopeSource) -> impl DerefMut<Target = Self> {
        let open_flow = !self.frames.in_flow();
        if open_flow {
            self.frames.enter_flow(source, self.scope_ids.next());
        }

        defer(self, move |this| {
            if open_flow {
                this.frames.exit_flow();
            }
        })
    }

    fn new_flow_guard(&mut self, source: ScopeSource) -> impl DerefMut<Target = Self> {
        self.frames.enter_flow(source, self.scope_ids.next());
        defer(self, |this| {
            this.frames.exit_flow();
        })
    }

    fn new_lexical_guard(&mut self, source: ScopeSource) -> impl DerefMut<Target = Self> {
        self.frames.enter_lexical(source, self.scope_ids.next());
        defer(self, |this| {
            this.frames.exit_lexical();
        })
    }

    fn resolve_type(&mut self, ident: Ident<'_>) {
        let name = ident.get();
        let expr_id = self
            .expr_id_table
            .get_expr_id(ident.span())
            .expect("span to exist in exprId table");
        let symbol_id = self
            .get(name.as_str())
            .or_else(|| primitive_type_alias(name.as_str()).and_then(|alias| self.get(alias)));

        let Some(symbol_id) = symbol_id else {
            self.unresolved_symbols.push(UnresolvedSymbol::new(
                name.clone(),
                expr_id,
                self.frames.current().source().clone(),
            ));
            trace_log!("unresolved type symbol {name} in {expr_id:?}");
            return;
        };

        self.symbol_usage.push(SymbolUsage { symbol_id, expr_id });
        self.expr_to_symbol.insert(expr_id, symbol_id);
        trace_log!("usage of type symbol {symbol_id:?} {name} in {expr_id:?}");

        let symbol = self.symbol(symbol_id);

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

    fn resolve_type_annotation(&mut self, annotation: Type<'_>) {
        self.resolve_type(annotation.ident());
        if let Some(args) = annotation.args() {
            for arg in args.items() {
                self.resolve_type_annotation(arg);
            }
        }
    }

    fn bind_type_param(&mut self, ty: Type<'_>) {
        let expr_id = self.expr_id(ty.ident().span());
        self.bind_ident_lexical(ty.ident(), SymbolOrigin::Param(expr_id))
            .with_kind(SymbolKind::Type);

        if let Some(args) = ty.args() {
            for arg in args.items() {
                self.bind_type_param(arg);
            }
        }
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
        trace_fn!("resolve expression", "expression: {}", expression.to_text());
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
                self_.resolve_code(code.items().collect(), code.statements().collect());
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
            Expr::PathAccess(path) => {
                self.resolve_expression(path.target());
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

                self_.resolve_code(lambda.items().collect(), lambda.statements().collect());
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

    fn declare_fn_items(&mut self, items: &[FnItem<'_>]) {
        for item in items {
            self.declare_fn_item(*item);
        }
    }

    fn declare_fn_item(&mut self, item: FnItem<'_>) {
        let function_expr_id = self.expr_id(item.name().span());
        self.bind_ident_lexical(item.name(), SymbolOrigin::Local(function_expr_id))
            .with_kind(SymbolKind::Function);
    }

    fn resolve_code(&mut self, items: Vec<FnItem<'_>>, statements: Vec<Statement<'_>>) {
        self.declare_fn_items(&items);

        for item in items {
            self.resolve_fn_item(item);
        }

        for statement in statements {
            self.resolve_statement(statement);
        }
    }

    fn resolve_fn_item(&mut self, item: FnItem<'_>) {
        let source = ScopeSource::expr(
            self.current_module_id(),
            self.expr_id_table.get_expr_id(item.span()).unwrap(),
        );
        let mut self_ = self.new_lexical_guard(source);

        for ty in item.type_args().items() {
            self_.bind_type_param(ty);
        }

        for param in item.params().children() {
            if let Some(annotation) = param.type_annotation() {
                self_.resolve_type_annotation(annotation);
            }

            if let ParamKind::Named(named) = param.kind() {
                self_.resolve_expression(named.expr());
            }
        }

        if let Some(return_type) = item.return_type() {
            self_.resolve_type_annotation(return_type);
        }

        for param in item.params().children() {
            match param.kind() {
                ParamKind::Pos(pattern) => {
                    self_.resolve_pattern(pattern, &mut |this, ident| {
                        let expr_id = this.expr_id(ident.span());
                        this.bind_ident_lexical(ident, SymbolOrigin::Param(expr_id));
                    });
                }
                ParamKind::Named(named) => {
                    let origin = SymbolOrigin::Param(self_.expr_id(named.name().span()));
                    self_.bind_ident_lexical(named.name(), origin);
                }
            }
        }

        self_.resolve_code(
            item.body().items().collect(),
            item.body().statements().collect(),
        );
    }

    pub fn resolve_statement(&mut self, statement: Statement<'_>) {
        match statement {
            Statement::Expr(expr) => self.resolve_expression(expr),
            Statement::Let(let_binding) => {
                if let Some(init) = let_binding.initial_value() {
                    self.resolve_expression(init);
                }

                if let Some(ty_annotation) = let_binding.type_annotation() {
                    self.resolve_type_annotation(ty_annotation)
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
            Statement::Break(break_) => {
                if let Some(value) = break_.value() {
                    self.resolve_expression(value);
                }
            }
            Statement::Return(ret) => {
                if let Some(value) = ret.value() {
                    self.resolve_expression(value);
                }
            }
            Statement::Continue(_) => {}
            Statement::ModuleImport(_) => {}
        }
    }
}

fn primitive_type_alias(name: &str) -> Option<&'static str> {
    match name {
        "Int" | "i32" | "i64" => Some("int"),
        "Bool" => Some("bool"),
        "Str" | "str" => Some("String"),
        "Unit" | "unit" => Some("()"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use compose_library::Library;
    use compose_library::diag::{FileError, FileResult};
    use compose_syntax::{FileId, Source};
    use std::collections::HashMap;
    use std::io::{Read, Write};
    use std::sync::Mutex;

    struct TestWorld {
        entrypoint: FileId,
        source: Mutex<HashMap<FileId, Source>>,
        library: Library,
    }

    impl TestWorld {
        fn from_str(text: &str) -> Self {
            let entrypoint = FileId::fake("test.cmps");
            let source = Source::new(entrypoint, text.to_owned());
            let mut sources = HashMap::new();
            sources.insert(entrypoint, source);
            Self {
                entrypoint,
                source: Mutex::new(sources),
                library: Library::default(),
            }
        }

        fn entry_source(&self) -> Source {
            self.source(self.entrypoint).expect("test source exists")
        }
    }

    impl World for TestWorld {
        fn entry_point(&self) -> FileId {
            self.entrypoint
        }

        fn source(&self, file_id: FileId) -> FileResult<Source> {
            self.source
                .lock()
                .expect("source lock")
                .get(&file_id)
                .cloned()
                .ok_or_else(|| FileError::NotFound(file_id.path().0.clone()))
        }

        fn library(&self) -> &Library {
            &self.library
        }

        fn write(
            &self,
            _f: &mut dyn FnMut(&mut dyn Write) -> std::io::Result<()>,
        ) -> std::io::Result<()> {
            Ok(())
        }

        fn read(
            &self,
            _f: &mut dyn FnMut(&mut dyn Read) -> std::io::Result<()>,
        ) -> std::io::Result<()> {
            Ok(())
        }
    }

    fn resolve(text: &str) -> ResolutionResult {
        let world = TestWorld::from_str(text);
        let source = world.entry_source();
        let mut expr_ids = ExprIdTable::new();
        expr_ids.visit_node(source.root_node());

        let mut resolver = NameResolver::new(&expr_ids, &world);
        resolver.set_emit_diagnostics(false);
        resolver.resolve()
    }

    #[test]
    fn resolves_fn_item_name_params_and_type_params() {
        let resolution = resolve(
            r#"
            fn id<T>(value: T) -> T {
                value
            }

            let out: Int = id(1);
            "#,
        );

        assert!(
            resolution.unresolved_symbols.is_empty(),
            "expected all fn item symbols to resolve, got {:#?}",
            resolution.unresolved_symbols
        );

        assert!(
            resolution
                .symbol_table
                .iter()
                .any(|(_, symbol)| symbol.name.as_str() == "id"
                    && symbol.symbol_kind == SymbolKind::Function),
            "expected `id` to be registered as a function"
        );
        assert!(
            resolution
                .symbol_table
                .iter()
                .any(|(_, symbol)| symbol.name.as_str() == "T"
                    && symbol.symbol_kind == SymbolKind::Type),
            "expected generic parameter `T` to be registered as a type"
        );
    }

    #[test]
    fn fn_items_are_block_scoped_and_order_independent() {
        let resolution = resolve(
            r#"
            let out: Int = {
                local(1);

                fn local(value: Int) -> Int {
                    value
                }
            };

            local(1);
            "#,
        );

        assert_eq!(
            resolution.unresolved_symbols.len(),
            1,
            "expected only the use outside the block to be unresolved, got {:#?}",
            resolution.unresolved_symbols
        );
        assert_eq!(resolution.unresolved_symbols[0].name.as_str(), "local");
    }
}
