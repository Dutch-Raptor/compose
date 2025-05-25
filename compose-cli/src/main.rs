use crate::error::CliError;
use crate::world::SystemWorld;
use clap::Parser;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{diagnostic, term};
use compose_eval::{Eval, Vm};
use compose_library::diag::error;
use compose_library::{
    Value,
    diag::{Severity, SourceDiagnostic, SourceResult, Warned},
};
use compose_syntax::ast::Expr;
use compose_syntax::{FileId, Source, Span};
use ecow::{eco_vec, EcoVec};
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
            primary_label(diag).into_iter().chain(
                diag.labels
                    .iter()
                    .flat_map(|l| Some(secondary_label(l.span)?.with_message(l.message.clone()))),
            ),
        );

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = term::Config::default();

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
pub fn eval(source: &Source, eval_range: Range<usize>, vm: &mut Vm) -> Warned<SourceResult<Value>> {
    let mut result = Value::unit();

    let range_start = min(eval_range.start, source.nodes().len());
    let range_end = min(eval_range.end, source.nodes().len());

    let nodes = source.nodes().get(range_start..range_end).unwrap();
    let errors = nodes
        .iter()
        .flat_map(|n| n.errors())
        .map(|e| e.into())
        .collect::<EcoVec<SourceDiagnostic>>();
    if !errors.is_empty() {
        return Warned::new(Err(errors));
    }
    for node in nodes {
        let expr: Expr = match node.cast() {
            Some(expr) => expr,
            None => {
                let span = node.span();
                let err = error!(span, "expected expression, found {:?}", node);
                return Warned::new(Err(eco_vec![err]))
                    .with_warnings(std::mem::take(&mut vm.sink.warnings));
            }
        };
        result = match expr.eval(vm) {
            Ok(value) => value,
            Err(err) => {
                return Warned::new(Err(err)).with_warnings(std::mem::take(&mut vm.sink.warnings));
            }
        }
    }

    Warned::new(Ok(result)).with_warnings(std::mem::take(&mut vm.sink.warnings))
}

pub struct RepeatIter<T: Clone> {
    item: T,
    count: usize,
}

impl<T: Clone> Iterator for RepeatIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.count == 0 {
            None
        } else {
            self.count -= 1;
            Some(self.item.clone())
        }
    }
}

impl<T> RepeatIter<T>
where
    T: Clone,
{
    pub fn new(item: T, count: usize) -> Self {
        Self { item, count }
    }
}

#[test]
fn test_repeat_iter() {
    let mut iter = RepeatIter::new(1, 3);
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), None);
}
