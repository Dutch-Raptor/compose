/*!
# Welcome to the Compose CLI!

This is the documentation for the Compose CLI.
For the documentation of the Compose language, see [the language docs](compose).)
*/
use crate::error::CliError;
use clap::Parser;
use compose_codespan_reporting::term;
use compose_codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use compose_eval::Machine;
use compose_library::diag::{write_diagnostics, SourceDiagnostic};
use compose_library::World;
use compose_resolve::{ExprIdTable, NameResolver};
use std::path::PathBuf;

mod error;
mod explain;
mod file;
mod repl;
mod world;

use crate::world::SystemWorld;
use compose_utils::ENABLE_TRACE;

#[derive(Debug, clap::Parser)]
#[command(version)]
struct Args {
    #[clap(subcommand)]
    pub command: Command,

    #[clap(long)]
    pub trace: bool,
}

#[derive(Debug, clap::Subcommand)]
enum Command {
    Repl(ReplArgs),
    File(FileArgs),
    Explain(ExplainArgs),
    Resolve(FileArgs),
}

#[derive(Debug, clap::Parser)]
pub struct FileArgs {
    pub file: PathBuf,

    #[clap(long)]
    /// Print the ast of the file before executing
    pub print_ast: bool,
    
    #[clap(long)]
    pub print_tokens: bool,
}

#[derive(Debug, clap::Parser)]
pub struct ReplArgs {
    #[clap(long)]
    pub print_ast: bool,

    #[clap(long)]
    pub debug: bool,

    #[clap(long)]
    /// Start REPL from this file.
    pub from: Option<PathBuf>,

    #[clap(long)]
    pub print_tokens: bool,
}

#[derive(Debug, clap::Parser)]
pub struct ExplainArgs {
    pub code: String,
}

fn main() -> Result<(), CliError> {
    let args = Args::parse();

    ENABLE_TRACE.store(args.trace, std::sync::atomic::Ordering::Relaxed);

    match args.command {
        Command::Repl(args) => repl::repl(args)?,
        Command::File(args) => file::file(args)?,
        Command::Explain(args) => explain::explain_command(args)?,
        Command::Resolve(args) => {
            let file = args.file;
            let world = SystemWorld::from_file(file)?;
            let source = world.entry_point_source()?;

            if args.print_ast {
                println!("{:#?}", source.root_node());
            }

            let mut expr_ids = ExprIdTable::new();
            expr_ids.visit_node(source.root_node());

            let mut name_resolver = NameResolver::new(&expr_ids, &world);
            name_resolver.resolve();
        }
    }

    Ok(())
}

pub fn print_diagnostics(
    world: &dyn World,
    errors: &[SourceDiagnostic],
    warnings: &[SourceDiagnostic],
) -> Result<(), compose_codespan_reporting::files::Error> {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();

    write_diagnostics(world, errors, warnings, &mut writer.lock(), &config)?;

    Ok(())
}