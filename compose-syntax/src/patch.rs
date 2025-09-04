use crate::span::HasSpan;
use crate::Span;
use ecow::{EcoString, EcoVec};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Patch {
    pub span: Span,
    pub replace: EcoString,
    pub message: Option<EcoString>,
}

impl Patch {
    pub fn apply(&self, source: &mut String) {
        let Some(range) = self.span.range() else {
            return;
        };
        source.replace_range(range, &self.replace);
    }
}

pub struct PatchEngine {
    patches: Vec<Patch>,
}

#[derive(Debug, Clone)]
pub enum PatchError {
    SpanWithoutRange,
    OutOfBounds,
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
        message: Option<impl Into<EcoString>>,
    ) -> Result<(), PatchError> {
        let span = node.get_span();
        let range = span.range().ok_or(PatchError::SpanWithoutRange)?;
        self.add_patch(Patch {
            span: span.with_range(range.start..range.start),
            replace: text.into(),
            message: message.map(|m| m.into()),
        });

        Ok(())
    }

    pub fn insert_after(
        &mut self,
        node: &impl HasSpan,
        text: impl Into<EcoString>,
        message: Option<impl Into<EcoString>>,
    ) -> Result<(), PatchError> {
        let span = node.get_span();
        let range = span.range().ok_or(PatchError::SpanWithoutRange)?;
        self.add_patch(Patch {
            span: span.with_range(range.end..range.end),
            replace: text.into(),
            message: message.map(|m| m.into()),
        });

        Ok(())
    }

    pub fn replace_node(
        &mut self,
        node: &impl HasSpan,
        text: impl Into<EcoString>,
        message: Option<impl Into<EcoString>>,
    ) -> Result<(), PatchError> {
        let span = node.get_span();
        self.add_patch(Patch {
            span,
            replace: text.into(),
            message: message.map(|m| m.into()),       
        });

        Ok(())
    }

    pub fn delete_node(&mut self, node: &impl HasSpan, message: Option<impl Into<EcoString>>) -> Result<(), PatchError> {
        self.add_patch(Patch {
            span: node.get_span(),
            replace: "".into(),
            message: message.map(|m| m.into()),      
        });

        Ok(())
    }

    pub(crate) fn get_patches(&self) -> EcoVec<Patch> {
        self.patches.iter().cloned().collect()
    }
}
