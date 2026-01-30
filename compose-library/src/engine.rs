use compose_library::World;
use compose_library::sink::Sink;
use compose_library::world::SyntaxContext;
use std::fmt::Debug;

pub struct Engine<'a> {
    pub world: &'a dyn World,
    pub sink: Sink,
}

impl<'a> Engine<'a> {
    pub fn syntax_ctx(&self) -> SyntaxContext<'_> {
        SyntaxContext { world: self.world }
    }
}

impl Debug for Engine<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Engine").field("sink", &self.sink).finish()
    }
}

pub struct Routes {}
