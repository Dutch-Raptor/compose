use crate::error::CliError;
use crate::world::SystemWorld;
use crate::FileArgs;
use compose_eval::Vm;
use compose_library::diag::Warned;

pub fn file(args: FileArgs) -> Result<(), CliError> {
    let file = args.file;
    let world = SystemWorld::from_file(file)?;
    let mut vm = Vm::new(&world);
    let source = world.entry_point_source()?;

    if args.print_ast {
        println!("AST: {:#?}\n", source.nodes());
    }

    let Warned { value, warnings } = compose::eval(&source, &mut vm);

    if let Err(err) = value {
        crate::print_diagnostics(&world, &err, &warnings).unwrap();
        return Err(CliError::ExecutionError);
    }

    crate::print_diagnostics(&world, &[], &warnings).unwrap();

    Ok(())
}
