use crate::FileArgs;
use crate::error::CliError;
use crate::world::SystemWorld;
use compose_eval::{EvalConfig, Vm};
use compose_library::diag::Warned;

pub fn file(args: FileArgs) -> Result<(), CliError> {
    let file = args.file;
    let world = SystemWorld::from_file(file)?;
    let mut vm = Vm::new(&world);
    let source = world.entry_point_source()?;

    if args.print_ast {
        println!("AST: {:#?}\n", source.nodes());
    }

    let warnings: Vec<_> = source.warnings().into_iter().map(|w| w.into()).collect();
    if !warnings.is_empty() {
        crate::print_diagnostics(&world, &[], &warnings).unwrap();
    }

    let Warned { value, warnings } = compose::eval(&source, &mut vm, &EvalConfig::default());

    if let Err(err) = value {
        crate::print_diagnostics(&world, &err, &warnings).unwrap();
        return Err(CliError::Execution);
    }

    crate::print_diagnostics(&world, &[], &warnings).unwrap();

    Ok(())
}
