use crate::error::CliError;
use crate::world::SystemWorld;
use clap::Parser;
use codespan_reporting::diagnostic::{Diagnostic, LabelStyle};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{diagnostic, term};
use compose_eval::Eval;
use compose_library::World;
use compose_library::diag::{Severity, SourceDiagnostic, write_diagnostics};
use compose_syntax::{FileId, Label, LabelType};
use ecow::eco_format;
use std::path::PathBuf;

mod error;
mod explain;
mod file;
mod repl;
mod world;

use compose_utils::ENABLE_TRACE;

#[derive(Debug, clap::Parser)]
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
}

#[derive(Debug, clap::Parser)]
pub struct FileArgs {
    pub file: PathBuf,

    #[clap(long)]
    /// Print the ast of the file before executing
    pub print_ast: bool,
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
        Command::Explain(args) => explain::explain(args)?,
    }

    Ok(())
}

pub fn print_diagnostics(
    world: &dyn World,
    errors: &[SourceDiagnostic],
    warnings: &[SourceDiagnostic],
) -> Result<(), codespan_reporting::files::Error> {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();

    write_diagnostics(world, errors, warnings, &mut writer.lock(), &config)?;

    Ok(())
}