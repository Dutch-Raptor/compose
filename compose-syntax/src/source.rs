use crate::file::FileId;
use crate::node::LinkedNode;
use crate::parser::parse_with_offset;
use crate::{parse, Span, SyntaxError, SyntaxNode};
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct Source(Arc<Repr>);

#[derive(Clone, Debug)]
struct Repr {
    id: FileId,
    text: String,
    nodes: Vec<SyntaxNode>,
    /// The byte indexes of the start of each line
    line_starts: Vec<usize>,
}

impl Source {
    pub fn from_file(path: impl Into<PathBuf>, text: String) -> Self {
        Self::new(FileId::new(path), text)
    }

    pub fn from_string(name: &str, text: impl Into<String>) -> Self {
        Self::new(FileId::fake(name), text.into())
    }

    pub fn new(file_id: FileId, text: String) -> Self {
        let line_starts = line_starts(&text, 0).collect();
        let nodes = parse(&text, file_id);
        Self(Arc::new(Repr {
            line_starts,
            id: file_id,
            text,
            nodes,
        }))
    }

    pub fn id(&self) -> FileId {
        self.0.id
    }

    pub fn text(&self) -> &str {
        &self.0.text
    }
    
    pub fn span_text(&self, span: Span) -> Option<&str> {
        if self.0.id != span.id()? {
            return None;
        }
        
        self.0.text.get(span.range()?)
    }

    pub fn nodes(&self) -> &[SyntaxNode] {
        &self.0.nodes
    }

    pub fn line_starts(&self) -> &[usize] {
        &self.0.line_starts
    }

    pub fn append(&mut self, text: &str) {
        let current_len = self.0.text.len();
        let id = self.0.id;
        let new_text = format!("{}{}", self.0.text, text);

        let inner = Arc::make_mut(&mut self.0);

        // parse the newly added text
        inner
            .nodes
            .extend(parse_with_offset(&new_text, id, current_len));
        inner.line_starts = line_starts(&new_text, 0).collect();
        inner.text = new_text;
    }

    pub fn warnings(&self) -> Vec<SyntaxError> {
        self.nodes().iter().flat_map(SyntaxNode::warnings).collect()
    }

    pub fn find(&self, span: Span) -> Option<LinkedNode<'_>> {
        for node in self.nodes() {
            let linked_node = LinkedNode::new(node);
            match linked_node.find(span) {
                Some(node) => return Some(node),
                None => continue,
            }
        }

        None
    }
}

pub fn line_starts(source: &str, offset: usize) -> impl '_ + Iterator<Item = usize> {
    core::iter::once(0).chain(source.match_indices('\n').map(move |(i, _)| i + 1 + offset))
}

impl AsRef<str> for Source {
    fn as_ref(&self) -> &str {
        self.text()
    }
}
