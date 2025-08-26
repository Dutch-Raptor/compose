use crate::span::HasSpan;
use crate::{Patch, PatchEngine, Span};
use ecow::{EcoString, EcoVec};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Fix {
    pub message: EcoString,
    pub patches: EcoVec<Patch>,
    pub display: FixDisplay,
    /// The span of the "pre-patched" source the fix is about
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum FixDisplay {
    Inline {
        /// The span to attach the label to
        span: Span
    },
    Footer,
}

pub struct FixBuilder {
    engine: PatchEngine,
    message: EcoString,
    display: FixDisplay,
    span: Span,
}

impl FixBuilder {
}

impl<'src> FixBuilder {
    pub fn new(message: impl Into<EcoString>, span: Span) -> Self {
        FixBuilder {
            engine: PatchEngine::new(),
            message: message.into(),
            display: FixDisplay::Footer,
            span,
        }
    }

    pub fn display(&mut self, display: FixDisplay) -> &mut FixBuilder {
        self.display = display;
        self
    }

    pub fn insert_before(&mut self, node: &impl HasSpan, text: &str) -> &mut FixBuilder {
        _ =self.engine.insert_before(node, text);
        self
    }

    pub fn insert_after(&mut self, node: &impl HasSpan, text: &str) -> &mut FixBuilder {
        _ = self.engine.insert_after(node, text);
        self
    }

    pub fn replace_node(&mut self, node: &impl HasSpan, text: &str) -> &mut FixBuilder {
        _ = self.engine.replace_node(node, text);
        self
    }

    pub fn delete_node(&mut self, node: &impl HasSpan) -> &mut FixBuilder {
        _ = self.engine.delete_node(node);
        self
    }

    pub fn add_patch(&mut self, patch: Patch) -> &mut FixBuilder {
        self.engine.add_patch(patch);
        self
    }

    pub fn build(&self) -> Fix {
        Fix {
            message: self.message.clone(),
            patches: self.engine.get_patches(),
            display: self.display.clone(),
            span: self.span,
        }
    }
}