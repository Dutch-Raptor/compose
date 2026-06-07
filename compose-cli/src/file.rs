use crate::FileArgs;
use crate::error::CliError;
use crate::repl::print_tokens;
use crate::world::SystemWorld;
use compose_eval::Machine;
use compose_library::diag::{SourceDiagnostic, Warned};
use compose_resolve::{ExprIdTable, NameResolver};
use compose_types::InferenceEngine;

pub fn file(args: FileArgs) -> Result<(), CliError> {
    let file = args.file;
    let world = SystemWorld::from_file(file)?;
    let mut vm = Machine::new(&world);
    let source = world.entry_point_source()?;

    if args.print_tokens {
        print_tokens(source.text(), source.id())
    }

    if args.print_ast {
        println!("AST: {:#?}\n", source.root_node());
    }

    let warnings: Vec<_> = source.warnings().into_iter().map(|w| w.into()).collect();
    if !warnings.is_empty() {
        crate::print_diagnostics(&world, &[], &warnings).expect("failed to print diagnostics");
    }

    let mut expr_ids = ExprIdTable::new();
    expr_ids.visit_node(source.root_node());

    let mut name_resolver = NameResolver::new(&expr_ids, &world);
    name_resolver.set_emit_diagnostics(false);
    let mut resolution = name_resolver.resolve();

    let mut static_errors = resolution.sink.take_errors().to_vec();
    static_errors.extend(
        resolution
            .unresolved_symbols
            .iter()
            .filter_map(|unresolved| {
                expr_ids.get_span(unresolved.expr_id).map(|span| {
                    SourceDiagnostic::error(
                        span,
                        format!("unresolved symbol `{}`", unresolved.name),
                    )
                    .with_label_message("this symbol could not be resolved")
                })
            }),
    );

    if !static_errors.is_empty() {
        crate::print_diagnostics(&world, &static_errors, &warnings)
            .expect("failed to print diagnostics");
        return Err(CliError::Execution);
    }

    let mut inference_engine = InferenceEngine::new(
        &world,
        &expr_ids,
        &resolution.symbol_table,
        &resolution.expr_to_symbol,
    );
    let type_result = inference_engine.infer();

    if !type_result.diagnostics.is_empty() {
        crate::print_diagnostics(&world, &type_result.diagnostics, &warnings)
            .expect("failed to print diagnostics");
        return Err(CliError::Execution);
    }

    let Warned { value, warnings } = compose_eval::eval_source(&source, &mut vm);

    if let Err(err) = value {
        crate::print_diagnostics(&world, &err, &warnings).expect("failed to print diagnostics");
        return Err(CliError::Execution);
    }

    crate::print_diagnostics(&world, &[], &warnings).expect("failed to print diagnostics");

    Ok(())
}
