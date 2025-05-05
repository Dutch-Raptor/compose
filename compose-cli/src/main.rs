use crate::world::SystemWorld;
use clap::Parser;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{diagnostic, term};
use compose_eval::{Eval, Vm};
use compose_library::{
    diag::{Severity, SourceDiagnostic, SourceResult, Warned},
    Value,
};
use compose_syntax::ast::Expr;
use compose_syntax::{FileId, Source, Span};
use rustyline::error::ReadlineError;
use std::cmp::min;
use std::ops::Range;

mod repl;
mod world;

#[derive(Debug, clap::Parser)]
struct Args {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(Debug, clap::Subcommand)]
enum Command {
    Repl(ReplArgs),
}

#[derive(Debug, clap::Parser)]
pub struct ReplArgs {
    #[clap(long)]
    pub print_ast: bool,
}

fn main() -> Result<(), ReadlineError> {
    let args = Args::parse();
    match args.command {
        Command::Repl(args) => repl::repl(args)?,
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
        .with_notes(diag.hints.iter().map(|h| format!("hint: {h}")).collect())
        .with_labels_iter(
            primary_label(diag).into_iter().chain(
                diag.labels
                    .iter()
                    .flat_map(|l| Some(secondary_label(l.span)?.with_message(l.message.clone()))),
            ),
        );

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        term::emit(&mut writer.lock(), &config, &world, &diagnostic)?;
    }

    Ok(())
}

pub fn primary_label(diag: &SourceDiagnostic) -> Option<diagnostic::Label<FileId>> {
    Some(diagnostic::Label::primary(
        diag.span.id()?,
        diag.span.range()?,
    ))
    .map(|l| match &diag.label_message {
        Some(message) => l.with_message(message),
        None => l,
    })
}

pub fn secondary_label(span: Span) -> Option<diagnostic::Label<FileId>> {
    Some(diagnostic::Label::secondary(span.id()?, span.range()?))
}

/// Eval a source file.
///
/// eval_range: eval these nodes
pub fn eval(source: Source, eval_range: Range<usize>, vm: &mut Vm) -> Warned<SourceResult<Value>> {
    let mut result = Value::Unit;

    let range_start = min(eval_range.start, source.nodes().len());
    let range_end = min(eval_range.end, source.nodes().len());

    for node in source.nodes().get(range_start..range_end).unwrap() {
        let expr: Expr = node.cast().unwrap();
        result = match expr.eval(vm) {
            Ok(value) => value,
            Err(err) => {
                return Warned::new(Err(err)).with_warnings(std::mem::take(&mut vm.sink.warnings));
            }
        }
    }

    Warned::new(Ok(result)).with_warnings(std::mem::take(&mut vm.sink.warnings))
}
