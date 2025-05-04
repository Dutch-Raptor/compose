use crate::world::SystemWorld;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{diagnostic, term};
use compose_eval::{Eval, Vm};
use compose_library::diag::{Severity, SourceDiagnostic, SourceResult, Warned};
use compose_library::{Value, World};
use compose_syntax::ast::Expr;
use compose_syntax::{FileId, Source, Span};
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::cmp::min;
use std::ops::Range;

mod world;

fn main() -> Result<(), ReadlineError> {
    let world = SystemWorld::from_str("");
    let mut vm = Vm::new(&world);

    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                eval_line(&mut vm, &world, line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

pub fn eval_line(vm: &mut Vm, world: &SystemWorld, line: String) {
    let len_before_edit = world.source(world.entry_point()).unwrap().nodes().len();
    world.edit_source(world.entry_point(), |s| {
        s.append(format!("{}{line}", if !s.text().is_empty() { "\n" } else { "" }).as_str())
    });

    let source = world.source(world.entry_point()).unwrap();
    let len_after_edit = source.nodes().len();

    let Warned { value, warnings } = eval(source, len_before_edit..len_after_edit, vm);

    match value {
        Ok(value) => {
            print_diagnostics(world, &[], &warnings).unwrap();

            if value != Value::Unit {
                println!("{value:?}")
            }
        }
        Err(err) => {
            print_diagnostics(world, &err, &warnings).unwrap();
        }
    }
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

        dbg!(&diag);

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
