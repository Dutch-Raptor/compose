use crate::file::{FileId, VirtualPath};
use std::path::PathBuf;
use std::sync::Arc;
use crate::{parse, SyntaxNode};
use crate::parser::parse_with_offset;

#[derive(Clone, Debug)]
pub struct Source(Arc<Repr>);

#[derive(Clone, Debug)]
struct Repr {
    id: FileId,
    text: String,
    nodes: Vec<SyntaxNode>,
    /// The byte indexes of the start of each line
    line_starts: Vec<usize>
}

impl Source {
    pub fn from_file(path: impl Into<PathBuf>, text: String) -> Self {
        Self::new(FileId::new(path), text)   
    }

    pub fn from_string(name: &str, text: String) -> Self {
        Self::new(FileId::fake(name), text)
    }
    
    pub fn new(file_id: FileId, text: String) -> Self {
        let line_starts = line_starts(&text, 0).collect();
        let nodes = parse(&text, file_id);
        Self(Arc::new(Repr {
            line_starts,
            id: file_id,
            text,
            nodes
        }))
    }
    
    pub fn id(&self) -> FileId {
        self.0.id
    }
    
    pub fn text(&self) -> &str {
        &self.0.text
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
        inner.nodes.extend(parse_with_offset(&new_text, id, current_len));
        inner.line_starts = line_starts(&new_text, 0).collect();
        inner.text = new_text;
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
