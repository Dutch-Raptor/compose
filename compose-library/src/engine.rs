use compose_library::{Routines, Sink, SyntaxContext, World};
use std::fmt::Debug;

pub struct Engine<'a> {
    pub routines: Routines,
    pub world: &'a dyn World,
    pub sink: Sink,
}

impl<'a> Engine<'a> {
    pub fn syntax_ctx(&self) -> SyntaxContext {
        SyntaxContext {
            world: self.world
        }
    }
}

impl Debug for Engine<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Engine")
            .field("sink", &self.sink)
            .finish()
    }
}

pub struct Routes {
    
}

