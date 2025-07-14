use crate::error::CliError;
use crate::repl::editor::{print_input, EditorFooter, EditorGutter, EditorHistory, EditorReader};
use crate::world::SystemWorld;
use crate::{explain, ReplArgs};
use compose_editor::editor::Editor;
use compose_editor::renderer::full::CrosstermRenderer;
use compose_eval::{EvalConfig, Machine};
use compose_library::diag::{eco_format, Warned};
use compose_library::{Value, World};
use compose_syntax::{FileId, Lexer, Source, SyntaxKind};
use std::fs;

mod editor;

pub fn repl(args: ReplArgs) -> Result<(), CliError> {
    let start_text = match &args.from {
        Some(path) => fs::read_to_string(path)?,
        None => String::new(),
    };

    let world = SystemWorld::from_str(&start_text);
    let mut vm = Machine::new(&world);

    if !start_text.is_empty() {
        // Show the initial source
        print_input(&start_text, 0);
        // evaluate the initial source
        eval_initial_pass(&mut vm, &world);
    }

    let history = EditorHistory::new();
    if !start_text.is_empty() {
        history.add(start_text);
    }

    println!("Welcome to the Compose REPL! Type :help for help.");

    loop {
        let source = entrypoint(&world);
        let line_count = match source.text().is_empty() {
            true => 0,
            false => source.line_starts().len(),
        };

        let renderer = CrosstermRenderer::default()
            .max_height(Some(10))
            .margin(EditorGutter {
                line_offset: line_count,
            })
            .footer(EditorFooter {
                message: "Hit enter to evaluate, Shift+Enter to insert newline, :q to quit",
            });

        let input = Editor::with_renderer(renderer)
            .read(EditorReader::new(&history))
            .map_err(|e| CliError::Editor(eco_format!("{e}")))?;

        match handle_repl_commands(&mut vm, &world, &input) {
            Some(ReplCommand::Quit) => break,
            Some(ReplCommand::Handled) => continue,
            None => {}
        }

        print_input(&input, line_count);

        eval_repl_input(&mut vm, &world, &input, &args);

        history.add(input);
    }

    Ok(())
}

fn entrypoint(world: &SystemWorld) -> Source {
    world.source(world.entry_point()).expect("No entrypoint")
}

fn print_help() {
    indoc::printdoc! {"
        Compose REPL Help
        =================
        This is an interactive REPL (Read-Eval-Print Loop) for experimenting with Compose code.

        Usage:
          • Type any valid Compose expression or statement and press Enter to evaluate it.
          • Use Shift+Enter to insert a newline (for multi-line input).
          • Commands (starting with ':') are available for interacting with the REPL.

        Available Commands:
          :q or :quit        Exit the REPL.
          :help              Show this help message.
          :vmstate           Print the current state of the virtual machine.
          :worldstate        Print the current state of the world.
          :ast               Print the current abstract syntax tree (AST).
          :save              Save the current REPL source buffer to a file.
          :explain <errcode> Show the explanation for a given error code.

        Tips:
          • The REPL maintains a running source buffer—each input is appended to the program.
          • You can inspect the AST and VM state at any time to better understand execution.
          • Use :save to persist your session code if you want to keep working on it later.
    "}
}

fn handle_repl_commands(vm: &mut Machine, world: &SystemWorld, input: &str) -> Option<ReplCommand> {
    match input.trim() {
        ":q" | ":quit" => return Some(ReplCommand::Quit),
        ":vmstate" => {
            println!("{vm:#?}\n");
        }
        ":worldstate" => {
            println!("{world:#?}\n");
        }
        ":ast" => {
            let source = entrypoint(world);
            let nodes = source.nodes();
            println!("AST: {:#?}\n", nodes);
        }
        ":help" => {
            print_help();
        }
        ":save" => {
            let renderer = CrosstermRenderer::default()
                .max_height(Some(10))
                .footer(EditorFooter {
                    message: "Please enter a filename to save to:",
                });

            let filename = match Editor::with_renderer(renderer)
                .read(EditorReader::new(&EditorHistory::new()))
            {
                Ok(name) => name,
                Err(e) => {
                    eprintln!("Error reading filename: {e}");
                    return Some(ReplCommand::Handled);
                }
            };

            let source = world.source(world.entry_point()).unwrap();
            let text = source.text();

            if let Err(e) = fs::write(&filename, text) {
                eprintln!("Failed to write to {filename}: {e}");
                return Some(ReplCommand::Handled);
            }
            println!("Saved to {filename}");
        }
        _ if input.starts_with(":explain ") => {
            let code = input.trim_start_matches(":explain ");
            _ = explain::explain(code);
        }
        _ => return None,
    }

    Some(ReplCommand::Handled)
}

enum ReplCommand {
    Quit,
    Handled,
}

fn eval_initial_pass(vm: &mut Machine, world: &SystemWorld) {
    let source = entrypoint(world);

    let warnings: Vec<_> = source.warnings().into_iter().map(|w| w.into()).collect();
    if !warnings.is_empty() {
        crate::print_diagnostics(world, &[], &warnings).unwrap();
    }

    // Evaluate every node in the source, printing any diagnostics along the way.
    // Do not return early if there are any errors, as we want to print all diagnostics.
    for i in 0..source.nodes().len() {
        let Warned { value, warnings } =
            compose_eval::eval_range(&source, i..i + 1, vm, &EvalConfig::default());
        crate::print_diagnostics(world, &[], &warnings).unwrap();
        if let Err(err) = value {
            crate::print_diagnostics(world, &err, &warnings).unwrap();
        }
    }
}

pub fn eval_repl_input(vm: &mut Machine, world: &SystemWorld, input: &str, args: &ReplArgs) {
    if input.is_empty() {
        return;
    }
    let len_before_edit = entrypoint(world).nodes().len();
    world.edit_source(world.entry_point(), |s| {
        s.append(format!("{}{input}", if !s.text().is_empty() { "\n" } else { "" }).as_str())
    });

    let source = entrypoint(world);
    let len_after_edit = source.nodes().len();
    
    if args.print_tokens {
        print_tokens(input, source.id());
    }

    if args.print_ast {
        let nodes = source.nodes().get(len_before_edit..len_after_edit).unwrap();
        println!("AST: {:#?}\n", nodes);
    }

    let syntax_warnings: Vec<_> = source.nodes()[len_before_edit..len_after_edit]
        .iter()
        .flat_map(|n| n.warnings())
        .map(|w| w.into())
        .collect();

    if !syntax_warnings.is_empty() {
        crate::print_diagnostics(world, &[], &syntax_warnings).unwrap();
    }

    let Warned { value, warnings } = compose_eval::eval_range(
        &source,
        len_before_edit..len_after_edit,
        vm,
        &EvalConfig::default(),
    );

    if args.debug {
        println!("{vm:#?}\n");
    }

    match value {
        Ok(value) => {
            crate::print_diagnostics(world, &[], &warnings).unwrap();

            if value != Value::unit() {
                println!("{value:?}")
            }
        }
        Err(err) => {
            crate::print_diagnostics(world, &err, &warnings).unwrap();
        }
    }
}

pub fn print_tokens(input: &str, file_id: FileId) {
    let mut l = Lexer::new(input, file_id);
    let mut tokens = Vec::new();
    loop {
        match l.next() {
            (SyntaxKind::End, _) => break,
            (_, node) => tokens.push(node),
        }
    }

    println!("Tokens: {:#?}\n", tokens);
}
