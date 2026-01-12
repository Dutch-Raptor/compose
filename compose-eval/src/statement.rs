use crate::vm::FlowEvent;
use crate::{eval, Eval, EvalConfig, Evaluated, Machine};
use compose_library::diag::{bail, error, SourceResult, Trace, TracePoint, Warned};
use compose_library::{Binding, IntoValue, Module};
use compose_syntax::ast::{AstNode, BreakStatement};
use compose_syntax::{ast, FileId};
use ecow::{eco_vec, EcoString};
use std::path::PathBuf;

impl Eval for ast::Statement<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        // trace_log!("eval statement: {:#?}", self);
        let guard = vm.temp_root_guard();
        let result = match self {
            ast::Statement::Expr(e) => e.eval(guard.vm),
            ast::Statement::Let(l) => l.eval(guard.vm),
            ast::Statement::Assign(a) => a.eval(guard.vm),
            ast::Statement::Break(b) => b.eval(guard.vm),
            ast::Statement::Return(r) => r.eval(guard.vm),
            ast::Statement::Continue(c) => c.eval(guard.vm),
            ast::Statement::ModuleImport(i) => i.eval(guard.vm),
        };

        guard.vm.maybe_gc();
        result
    }
}

impl Eval for BreakStatement<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        match self.value() {
            None => {
                vm.flow = Some(FlowEvent::Break(self.span(), None));
            }
            Some(expr) => {
                let value = expr.eval(vm)?.value;
                vm.flow = Some(FlowEvent::Break(self.span(), Some(value)))
            }
        }

        Ok(Evaluated::unit())
    }
}

impl Eval for ast::ReturnStatement<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        match self.value() {
            None => {
                vm.flow = Some(FlowEvent::Return(self.span(), None));
            }
            Some(expr) => {
                let value = expr.eval(vm)?.value;
                vm.flow = Some(FlowEvent::Return(self.span(), Some(value)))
            }
        }

        Ok(Evaluated::unit())
    }
}

impl Eval for ast::ContinueStatement<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        vm.flow = Some(FlowEvent::Continue(self.span()));
        Ok(Evaluated::unit())
    }
}

impl Eval for ast::ModuleImport<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let source = self.source();

        let path = match self.span().resolve_path(source.as_str()) {
            Some(p) => p,
            None => bail!(
                self.source_span(),
                "could not resolve module path from a file without path information"
            ),
        };

        let name = match self.alias() {
            None => {
                let as_path = PathBuf::from(source.as_str());
                let stem = match as_path.file_stem() {
                    Some(stem) => stem,
                    None => bail!(self.source_span(), "could not resolve module name from path")
                };
                EcoString::from(stem.to_string_lossy().as_ref())
            }
            Some(name) => name.get().to_owned()
        };

        let id = FileId::new(path);

        let source = vm.engine.world.source(id).map_err(|e| {
            eco_vec!(error!(
                self.source_span(),
                "could not load module source: {}", e
            ))
        })?;

        let module = vm.with_frame(|vm| {
            let Warned { value, warnings } = eval(&source, vm, &EvalConfig::default());
            value?;
            vm.sink_mut().warnings.extend(warnings);


            let module = Module::new(name.clone(), vm.frames.top.scopes.top.clone());
            SourceResult::Ok(module.into_value())
        }).trace(|| TracePoint::Import, self.source_span())?;

        vm.try_bind(name, Binding::new(module, self.span()))?;

        Ok(Evaluated::unit())
    }
}
