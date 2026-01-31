use crate::evaluated::Evaluated;
use crate::vm::FlowEvent;
use crate::{Eval, Machine, eval_source};
use compose_library::diag::{SourceResult, Trace, TracePoint, Warned, bail, error};
use compose_syntax::ast::{AstNode, BreakStatement};
use compose_syntax::{FileId, ast};
use ecow::{EcoString, eco_vec};
use std::path::PathBuf;
use compose_library::foundations::cast::IntoValue;
use compose_library::foundations::module::Module;
use compose_library::foundations::scope::Binding;

impl Eval for ast::Statement<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let result = {
            // Flow scope can be dropped after evaluating the statement, this way it can be GCd
            let vm = &mut vm.new_flow_scope_guard();
            match self {
                ast::Statement::Expr(e) => e.eval(vm),
                ast::Statement::Let(l) => l.eval(vm),
                ast::Statement::Assign(a) => a.eval(vm),
                ast::Statement::Break(b) => b.eval(vm),
                ast::Statement::Return(r) => r.eval(vm),
                ast::Statement::Continue(c) => c.eval(vm),
                ast::Statement::ModuleImport(i) => i.eval(vm),
            }
        };

        let vm = &mut vm.temp_root_guard();
        vm.temp_root_scope(|vm| {
            if let Ok(result) = &result {
                vm.track_tmp_root(result.value());
            }

            vm.maybe_gc();
            Ok(())
        })?;

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
                    None => bail!(
                        self.source_span(),
                        "could not resolve module name from path"
                    ),
                };
                EcoString::from(stem.to_string_lossy().as_ref())
            }
            Some(name) => name.get().to_owned(),
        };

        let id = FileId::new(path);

        let source = vm.engine.world.source(id).map_err(|e| {
            eco_vec!(error!(
                self.source_span(),
                "could not load module source: {}", e
            ))
        })?;

        let module = vm
            .with_frame(|vm| {
                let Warned { value, warnings } = eval_source(&source, vm);
                value?;
                vm.sink_mut().warnings.extend(warnings);

                let module = Module::new(name.clone(), vm.scopes().top_lexical().clone());
                SourceResult::Ok(module.into_value())
            })
            .trace(|| TracePoint::Import, self.source_span())?;

        vm.try_bind(name, Binding::new(module, self.span()))?;

        Ok(Evaluated::unit())
    }
}
