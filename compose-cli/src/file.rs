use crate::error::CliError;
use crate::world::SystemWorld;
use crate::FileArgs;
use compose_eval::{EvalConfig, Machine};
use compose_library::diag::Warned;
use crate::repl::print_tokens;

pub fn file(args: FileArgs) -> Result<(), CliError> {
    let file = args.file;
    let world = SystemWorld::from_file(file)?;
    let mut vm = Machine::new(&world);
    let source = world.entry_point_source()?;
    
    if args.print_tokens {
        print_tokens(source.text(), source.id())
    }

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
