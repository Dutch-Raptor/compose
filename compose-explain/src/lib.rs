use std::fmt::Write;
use crate::world::ExplainWorld;
use codespan_reporting::term::Config;
use codespan_reporting::term::termcolor::Ansi;
use compose_error_codes::ErrorCode;
use compose_eval::{EvalConfig, Vm};
use compose_library::Value;
use compose_library::diag::{Warned, write_diagnostics};
use pulldown_cmark::{CodeBlockKind, Event, Tag, TagEnd};
use regex::Regex;

mod world;

pub trait Explain {
    fn explain(&self) -> String;
}

impl Explain for ErrorCode {
    fn explain(&self) -> String {
        process_markdown(self.description)
    }
}

/// Processes Markdown text by executing special code blocks marked with 'compose' and injecting their output.
///
/// Takes Markdown text as input and processes it, looking for fenced code blocks that start with
/// `compose label` where `label` is any non-whitespace string. When such a block is found:
/// 1. The code inside the block is executed using the Compose language interpreter
/// 2. The original code block is preserved with only "compose" as its info string
/// 3. A new code block with "output label" is inserted after it containing the execution results
///
/// # Arguments
///
/// * `input` - A string slice containing Markdown text to process
///
/// # Returns
///
/// A String containing the processed Markdown with executed code blocks and their outputs
///
/// # Example
///
/// ````markdown
/// Here's a code block:
/// ```compose example
/// 1 + 1
/// ```
///
/// Will become:
/// ```compose
/// 1 + 1
/// ```
/// ```output example
/// 2
/// ```
/// ````
fn process_markdown(input: &str) -> String {
    let parser = pulldown_cmark::Parser::new(input);
    let compose_block_re = Regex::new(r"^compose\s+(\S+)$").expect("is a valid regex");

    let mut in_compose_block = false;
    let mut current_label = String::new();
    let mut code_buffer = String::new();

    let mut events: Vec<Event<'_>> = Vec::new();

    for event in parser {
        match &event {
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(info))) => {
                if let Some(captures) = compose_block_re.captures(info) {
                    in_compose_block = true;
                    current_label = captures[1].to_string();
                    code_buffer.clear();

                    // Push a cleaned code block (`compose`)
                    events.push(Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(
                        "compose".into(),
                    ))));
                } else {
                    events.push(event.clone());
                }
            }

            Event::Text(text) => {
                if in_compose_block {
                    code_buffer.push_str(text);
                }
                events.push(event.clone());
            }

            Event::End(TagEnd::CodeBlock) => {
                events.push(event.clone());

                if in_compose_block {
                    in_compose_block = false;

                    let result = execute_with_diagnostics(&code_buffer);

                    // Inject output block events
                    events.push(Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(
                        format!("output {current_label}").into(),
                    ))));
                    events.push(Event::Text(result.into()));
                    events.push(Event::End(TagEnd::CodeBlock));
                }
            }

            _ => {
                events.push(event.clone());
            }
        }
    }

    let mut result = String::new();
    pulldown_cmark_to_cmark::cmark(events.into_iter(), &mut result).expect("invalid Markdown");
    result
}

fn execute_with_diagnostics(code: &str) -> String {
    let world = ExplainWorld::from_str(code);
    let mut vm = Vm::new(&world);

    let mut diags_buffer = vec![];
    let mut wtr = Ansi::new(&mut diags_buffer);
    let config = Config::default();

    let mut output = String::new();

    let warnings: Vec<_> = world
        .source
        .warnings()
        .into_iter()
        .map(Into::into)
        .collect();

    if !warnings.is_empty() {
        let mut warn_buffer = vec![];
        let mut wtr = Ansi::new(&mut warn_buffer);
        write_diagnostics(&world, &[], &warnings, &mut wtr, &config)
            .expect("failed to write diagnostics");
        output.push_str(
            String::from_utf8(warn_buffer)
                .expect("invalid string")
                .trim(),
        );
    }

    let Warned { value, warnings } =
        compose_eval::eval(&world.source, &mut vm, &EvalConfig::default());

    let stdout = world.stdout.lock().expect("failed to lock stdout");

    let sep = |s: &mut String| {
        if !s.is_empty() {
            s.push('\n');
        }
    };

    match value {
        Ok(value) => {
            write_diagnostics(&world, &[], &warnings, &mut wtr, &config)
                .expect("failed to write diagnostics");

            if !diags_buffer.is_empty() {
                sep(&mut output);
                output.push_str(
                    String::from_utf8(diags_buffer)
                        .expect("invalid string")
                        .trim(),
                );
            }

            if !stdout.is_empty() {
                sep(&mut output);
                sep(&mut output);
                output.push_str("stdout:\n");
                output.push_str(stdout.as_str());
                drop(stdout);
            }

            if value != Value::unit() {
                sep(&mut output);
                write!(output, "{value:?}").expect("writing to string is infallible");
            }
            output
        }
        Err(err) => {
            write_diagnostics(&world, &err, &warnings, &mut wtr, &config)
                .expect("failed to write diagnostics");

            if !stdout.is_empty() {
                sep(&mut output);
                output.push_str("stdout:\n");
                output.push_str(stdout.as_str());
                drop(stdout);
            }

            if !diags_buffer.is_empty() {
                sep(&mut output);
                output.push_str(
                    String::from_utf8(diags_buffer)
                        .expect("invalid string")
                        .trim(),
                );
            }
            output
        }
    }
}
