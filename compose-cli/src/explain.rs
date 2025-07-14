use crate::ExplainArgs;
use crate::error::CliError;
use compose_doc::Config;

pub fn explain_command(args: ExplainArgs) -> Result<(), CliError> {
    explain(&args.code)
}

pub(crate) fn explain(code: &str) -> Result<(), CliError> {
    match compose_error_codes::lookup(code) {
        None => {
            eprintln!("No error code found for {}", code);
            Err(CliError::Execution)
        }
        Some(code) => {
            let md_text = code.description;

            let explained = match compose_doc::transform_markdown(md_text, &Config::ansi()) {
                Ok(v) => v,
                Err(e) => {
                    eprintln!(
                        "Something went wrong while transforming the markdown: {}",
                        e.message
                    );
                    return Err(CliError::Execution);
                }
            };

            println!("{}", explained);
            Ok(())
        }
    }
}
