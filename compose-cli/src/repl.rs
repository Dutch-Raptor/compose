use crate::ReplArgs;
use crate::world::SystemWorld;
use compose_eval::Vm;
use compose_library::diag::Warned;
use compose_library::{Value, World};
use rustyline::error::ReadlineError;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{
    Cmd, Completer, DefaultEditor, Editor, EventHandler, Helper, Highlighter, Hinter, KeyEvent,
    Modifiers, Validator,
};

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct InputValidator {
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
}

pub fn repl(args: ReplArgs) -> Result<(), ReadlineError> {
    let world = SystemWorld::from_str("");
    let mut vm = Vm::new(&world);

    // `()` can be used when no completer is required
    let mut rl = Editor::new()?;
    let h = InputValidator {
        brackets: MatchingBracketValidator::new(),
        highlighter: MatchingBracketHighlighter::new(),
    };
    rl.set_helper(Some(h));
    rl.bind_sequence(
        KeyEvent::new('\r', Modifiers::SHIFT),
        EventHandler::Simple(Cmd::Newline),
    );
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                eval_line(&mut vm, &world, line, &args);
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

pub fn eval_line(vm: &mut Vm, world: &SystemWorld, line: String, args: &ReplArgs) {
    let len_before_edit = world.source(world.entry_point()).unwrap().nodes().len();
    world.edit_source(world.entry_point(), |s| {
        s.append(format!("{}{line}", if !s.text().is_empty() { "\n" } else { "" }).as_str())
    });

    let source = world.source(world.entry_point()).unwrap();
    let len_after_edit = source.nodes().len();

    if args.print_ast {
        let nodes = source.nodes().get(len_before_edit..len_after_edit).unwrap();
        println!("AST: {:#?}", nodes);
    }

    let Warned { value, warnings } = crate::eval(source, len_before_edit..len_after_edit, vm);

    match value {
        Ok(value) => {
            crate::print_diagnostics(world, &[], &warnings).unwrap();

            if value != Value::Unit {
                println!("{value:?}")
            }
        }
        Err(err) => {
            crate::print_diagnostics(world, &err, &warnings).unwrap();
        }
    }
}
