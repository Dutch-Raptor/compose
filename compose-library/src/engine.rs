use compose_library::{Routines, Sink, World};
use std::fmt::Debug;

pub struct Engine<'a> {
    pub routines: Routines,
    pub world: &'a dyn World,
    pub sink: Sink,
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

