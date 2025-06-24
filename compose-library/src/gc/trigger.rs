use compose_library::gc::clean::CleanResult;
use std::fmt::Debug;

pub trait GcTriggerPolicy: Debug {
    /// Should this event trigger a clean?
    fn on_event(&mut self, event: &GcEvent, gc_data: &GcData) -> bool;

    fn after_gc(&mut self, clean_result: &CleanResult, gc_data: &GcData);
}
pub enum GcEvent {
    MaybeGc,
}

#[derive(Debug)]
pub struct GcData {
    pub heap_size: usize,
}

#[derive(Debug)]
pub struct SimplePolicy {
    pub(crate) heap_size_threshold: usize,
}

impl GcTriggerPolicy for SimplePolicy {
    fn on_event(&mut self, _: &GcEvent, gc_data: &GcData) -> bool {
        gc_data.heap_size > self.heap_size_threshold
    }

    fn after_gc(&mut self, clean_result: &CleanResult, gc_data: &GcData) {
        dbg!(clean_result, gc_data);
    }
}
