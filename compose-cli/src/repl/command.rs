use std::path::PathBuf;

pub enum ReplCommand {
    Quit,
    Help,
    VmState,
    WorldState,
    Ast,
    Save(SaveArgs)
}


fn parse_command(input: &str) -> Option<ReplCommand> {
    if !input.starts_with(':') {
        return None;
    }

    match input {
        ":q" | ":quit" => Some(ReplCommand::Quit),
        ":h" | ":help" => Some(ReplCommand::Help),
        ":vms" => Some(ReplCommand::VmState),
        ":ws" => Some(ReplCommand::WorldState),
        ":ast" => Some(ReplCommand::Ast),
        // _ if input.starts_with(":save") => parse_save_command(input),
        _ => None,
    }
}



pub struct SaveArgs {
    pub file: PathBuf
}