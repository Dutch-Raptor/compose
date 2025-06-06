use crate::error::CliError;
use crate::ExplainArgs;
use compose_explain::Explain;

pub fn explain(args: ExplainArgs) -> Result<(), CliError> {
    match compose::error_codes::lookup(&args.code) {
        None => {
            eprintln!("No error code found for {}", args.code);
            Err(CliError::Execution)
        }
        Some(code) => {
            let explained = code.explain();
            println!("{}", explained);
            Ok(())
        }
    }
}
