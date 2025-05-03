use crate::file::{FileId, VirtualPath};
use std::path::PathBuf;
use std::sync::Arc;

pub struct Source(Arc<Repr>);

struct Repr {
    id: FileId,
    text: String,
}

impl Source {
    pub fn from_file(path: impl Into<PathBuf>, text: String) -> Self {
        Self(Arc::new(Repr {
            id: FileId::new(VirtualPath::new(path)),
            text,
        }))
    }

    pub fn from_string(name: &str, text: String) -> Self {
        Self(Arc::new(Repr {
            id: FileId::fake(VirtualPath::new(name)),
            text,
        }))
    }
}
