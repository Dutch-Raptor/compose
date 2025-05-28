use crate::error::CliError;
use crate::world::SystemWorld;
use clap::Parser;
use clap::builder::styling::Style;
use codespan_reporting::diagnostic::{Diagnostic, LabelStyle};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{diagnostic, term};
use compose_eval::{Eval, Vm};
use compose_library::diag::error;
use compose_library::{
    Value,
    diag::{Severity, SourceDiagnostic, SourceResult, Warned},
};
use compose_syntax::ast::{Expr, Statement};
use compose_syntax::{FileId, Label, LabelType, Source, Span};
use ecow::{EcoVec, eco_vec};
use std::cmp::min;
use std::ops::Range;
use std::path::PathBuf;

mod error;
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

fn main() -> Result<(), CliError> {
    let args = Args::parse();

    ENABLE_TRACE.store(args.trace, std::sync::atomic::Ordering::Relaxed);

    match args.command {
        Command::Repl(args) => repl::repl(args)?,
        Command::File(args) => file::file(args)?,
    }

    Ok(())
}

pub fn print_diagnostics(
    world: &SystemWorld,
    errors: &[SourceDiagnostic],
    warnings: &[SourceDiagnostic],
) -> Result<(), codespan_reporting::files::Error> {
    for diag in warnings.iter().chain(errors) {
        let diagnostic = match diag.severity {
            Severity::Error => Diagnostic::error(),
            Severity::Warning => Diagnostic::warning(),
        }
        .with_message(diag.message.clone())
        .with_notes(diag.notes.iter().map(|n| format!("note: {n}")).collect())
        .with_notes(diag.hints.iter().map(|h| format!("help: {h}")).collect())
        .with_labels_iter(
            diag_label(diag)
                .into_iter()
                .chain(diag.labels.iter().flat_map(create_label)),
        );

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = term::Config::default();

        term::emit(&mut writer.lock(), &config, &world, &diagnostic)?;
    }

    Ok(())
}

pub fn diag_label(diag: &SourceDiagnostic) -> Option<diagnostic::Label<FileId>> {
    let id = diag.span.id()?;
    let range = diag.span.range()?;

    let mut label = diagnostic::Label::primary(id, range);
    if let Some(message) = diag.label_message.as_ref() {
        label = label.with_message(message);
    }
    
    Some(label)
}

pub fn create_label(label: &Label) -> Option<diagnostic::Label<FileId>> {
    let id = label.span.id()?;
    let range = label.span.range()?;
    let style = match label.ty {
        LabelType::Primary => LabelStyle::Primary,
        LabelType::Secondary => LabelStyle::Secondary,
    };

    Some(diagnostic::Label::new(style, id, range).with_message(label.message.clone()))
}
