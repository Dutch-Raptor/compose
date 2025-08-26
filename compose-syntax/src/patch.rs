use crate::span::HasSpan;
use ecow::{EcoString, EcoVec};
use std::ops::Range;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Patch {
    Insert {
        // byte index to insert at within the source
        at: usize,
        text: EcoString,
    },
    Replace {
        // byte range to replace within the source
        range: Range<usize>,
        text: EcoString,
    },
    Delete {
        // byte range to delete within the source
        range: Range<usize>,
    },
}

impl Patch {
    pub fn apply(&self, source: &mut String) {
        match self {
            Patch::Insert { at, text } => source.insert_str(*at, text),
            Patch::Replace { range, text } => source.replace_range(range.clone(), text),
            Patch::Delete { range } => source.replace_range(range.clone(), ""),
        }
    }

    /// Return a new Patch shifted by `delta` bytes (positive or negative),
    /// or `None` if the shift would result in invalid (negative) positions.
    pub fn shifted(self, delta: isize) -> Result<Patch, PatchError> {
        fn shift_pos(pos: usize, delta: isize) -> Result<usize, PatchError> {
            let new = pos as isize + delta;
            if new < 0 { Err(PatchError::OutOfBounds) } else { Ok(new as usize) }
        }

        Ok(match self {
            Patch::Insert { at, text } => Patch::Insert {
                at: shift_pos(at, delta)?,
                text,
            },
            Patch::Replace { range, text } => Patch::Replace {
                range: shift_pos(range.start, delta)?..shift_pos(range.end, delta)?,
                text,
            },
            Patch::Delete { range } => Patch::Delete {
                range: shift_pos(range.start, delta)?..range.end,
            },
        })
    }

    fn range(&self) -> Range<usize> {
        match self {
            Patch::Insert { at, .. } => *at..*at,
            Patch::Replace { range, .. } => range.clone(),
            Patch::Delete { range } => range.clone(),
        }
    }

    fn start_pos(&self) -> usize {
        match self {
            Patch::Insert { at, .. } => *at,
            Patch::Replace { range, .. } => range.start,
            Patch::Delete { range } => range.start,
        }
    }

    fn conflicts_with(&self, other: &Patch) -> bool {
        let r1 = self.range();
        let r2 = other.range();

        if r1.end > r2.start && r2.end > r1.start {
            return true;
        }

        if let Patch::Insert { at, .. } = self {
            if *at > r2.start && *at < r2.end {
                return true;
            }
        }
        if let Patch::Insert { at, .. } = other {
            if *at > r1.start && *at < r1.end {
                return true;
            }
        }

        false
    }
}

pub struct PatchEngine {
    patches: Vec<Patch>,
}

impl PatchEngine {
}

#[derive(Debug, Clone)]
pub enum PatchError {
    SpanWithoutRange,
    OutOfBounds,
    Conflict {
        a: Arc<Patch>,
        b: Arc<Patch>,
    }
}

impl PatchEngine {
    pub fn new() -> Self {
        Self { patches: vec![] }
    }

    pub fn add_patch(&mut self, patch: Patch) {
        self.patches.push(patch);
    }

    pub fn add_patches(&mut self, patches: impl IntoIterator<Item = Patch>) {
        self.patches.extend(patches);
    }

    pub fn insert_before(
        &mut self,
        node: &impl HasSpan,
        text: impl Into<EcoString>,
    ) -> Result<(), PatchError> {
        let span = node.span();
        self.add_patch(Patch::Insert {
            at: span.range().ok_or(PatchError::SpanWithoutRange)?.start,
            text: text.into(),
        });

        Ok(())
    }

    pub fn insert_after(
        &mut self,
        node: &impl HasSpan,
        text: impl Into<EcoString>,
    ) -> Result<(), PatchError> {
        let span = node.span();
        self.add_patch(Patch::Insert {
            at: span.range().ok_or(PatchError::SpanWithoutRange)?.end,
            text: text.into(),
        });

        Ok(())
    }

    pub fn replace_node(
        &mut self,
        node: &impl HasSpan,
        text: impl Into<EcoString>,
    ) -> Result<(), PatchError> {
        let span = node.span();
        self.add_patch(Patch::Replace {
            range: span.range().ok_or(PatchError::SpanWithoutRange)?,
            text: text.into(),
        });

        Ok(())
    }

    pub fn delete_node(&mut self, node: &impl HasSpan) -> Result<(), PatchError> {
        self.add_patch(Patch::Delete {
            range: node.span().range().ok_or(PatchError::SpanWithoutRange)?,
        });

        Ok(())
    }

    fn check_conflicts(&self) -> Result<(), PatchError> {
        for (i, p1) in self.patches.iter().enumerate() {
            for p2 in self.patches.iter().skip(i + 1) {
                if p1.conflicts_with(p2) {
                    return Err(PatchError::Conflict {
                        a: Arc::new(p1.clone()),
                        b: Arc::new(p2.clone()),
                    });
                }
            }
        }
        Ok(())
    }

    pub fn apply_all(self, source: &str) -> Result<String, PatchError> {
        self.apply_all_with_offset(source, 0)
    }

    pub fn apply_all_with_offset(mut self, snippet: &str, base_offset: usize) -> Result<String, PatchError> {
        self.check_conflicts()?;

        let mut result = snippet.to_string();
        self.patches.sort_by_key(|p| p.start_pos());
        self.patches.reverse();

        let offset = -(base_offset as isize);
        for patch in self.patches.iter().filter(|p| {
            p.range().start >= base_offset && p.range().end <= base_offset + snippet.len()
        }) {
            patch.clone().shifted(offset)?.apply(&mut result);
        }

        Ok(result)
    }

    pub(crate) fn get_patches(&self) -> EcoVec<Patch> {
        self.patches.iter().cloned().collect()
    }
}
