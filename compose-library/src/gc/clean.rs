use compose_library::gc::trigger::GcEvent;
use compose_library::{Heap, Trace};
use slotmap::SecondaryMap;
use std::collections::VecDeque;
use std::time::{Duration, Instant};

impl Heap {
    pub fn maybe_gc(&mut self, root: &impl Trace) -> Option<CleanResult> {
        if !self.policy.on_event(&GcEvent::MaybeGc, &self.data()) {
            return None;
        }
        Some(self.clean(root))
    }

    pub fn clean(&mut self, root: &impl Trace) -> CleanResult {
        let start = Instant::now();
        let mut worklist = VecDeque::new();

        root.visit_refs(&mut |key| worklist.push_back(key));

        // Mark phase
        let mut marked = SecondaryMap::new();
        while let Some(key) = worklist.pop_front() {
            if marked.insert(key, ()).is_some() {
                // already marked
                continue;
            }

            let Some(item) = self.map.get(key) else {
                // already cleaned
                continue;
            };

            item.visit_refs(&mut |key| worklist.push_back(key));
        }

        let mut sweeped = 0;
        let total_allocated = self.map.len();

        // Sweep phase
        self.map.retain(|key, _| {
            let keep = marked.contains_key(key);
            if !keep {
                sweeped += 1;
            }
            keep
        });

        let gc_duration = start.elapsed();

        CleanResult {
            sweeped,
            total_allocated,
            gc_duration,
        }
    }
}

#[derive(Debug)]
pub struct CleanResult {
    /// The amount of items removed
    pub sweeped: usize,
    /// The amount of allocated items, before cleaning
    pub total_allocated: usize,
    pub gc_duration: Duration,
}

impl CleanResult {
    pub fn marked(&self) -> usize {
        self.total_allocated - self.sweeped
    }
}
