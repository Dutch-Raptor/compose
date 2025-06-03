use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroU16;
use std::path::PathBuf;
use std::sync::{LazyLock, RwLock};

static INTERNER: LazyLock<RwLock<FileInterner>> = LazyLock::new(|| {
    RwLock::new(FileInterner {
        from_id: Vec::new(),
        to_id: HashMap::new(),
    })
});

/// Resets the interner for file ids.
///
/// # Warning
///
/// This will break existing file ids and cause any reading of paths from invalidated file ids to panic!
/// Only use this if you know you will not use any of the existing file ids.
pub fn reset_interner() {
    *INTERNER.write().unwrap() = FileInterner {
        from_id: Vec::new(),
        to_id: HashMap::new(),
    }
}

struct FileInterner {
    from_id: Vec<FileRef>,
    to_id: HashMap<FileRef, FileId>,
}

type FileRef = &'static VirtualPath;

/// An absolute path in the virtual file system of a project
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct VirtualPath(pub PathBuf);

impl<T> From<T> for VirtualPath 
where T: Into<PathBuf>
{
    fn from(value: T) -> Self {
        Self(value.into())   
    }
}

impl VirtualPath {
    /// Create a new virtual path.
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self(path.into())
    }

    pub fn display(&self) -> String {
        self.0.display().to_string()
    }
}

impl Debug for VirtualPath {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.0.display(), f)
    }
}

/// Identifier for a file,
///
/// Globally interned and thus cheap to copy, compare, and hash.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct FileId(NonZeroU16);

impl FileId {
    #[track_caller]
    pub fn new(path: impl Into<VirtualPath>) -> Self {
        // Check if the file is already in the interner.
        let path = path.into();
        let mut interner = INTERNER.write().unwrap();
        if let Some(&id) = interner.to_id.get(&path) {
            return id;
        }

        let num = u16::try_from(interner.from_id.len() + 1)
            .and_then(NonZeroU16::try_from)
            .expect("out of file ids");

        let id = FileId(num);
        let leaked = Box::leak(Box::new(path));
        interner.to_id.insert(leaked, id);
        interner.from_id.push(leaked);
        id
    }

    /// Create a fake file id for a virtual path.
    ///
    /// Always returns a new file id, no matter if the file is already in the interner.
    #[track_caller]
    pub fn fake(path: impl Into<VirtualPath>) -> Self {
        let mut interner = INTERNER.write().unwrap();
        let num = u16::try_from(interner.from_id.len() + 1)
            .and_then(NonZeroU16::try_from)
            .expect("out of file ids");

        let id = FileId(num);
        let leaked = Box::leak(Box::new(path.into()));
        interner.from_id.push(leaked);
        id
    }

    #[track_caller]
    pub fn path(&self) -> &'static VirtualPath {
        self.try_path().expect("file id not interned")
    }

    pub fn try_path(&self) -> Option<&'static VirtualPath> {
        let interner = INTERNER.try_read().ok()?;
        let id = usize::try_from(self.0.get() - 1)
            .ok()?;

        interner.from_id.get(id).copied()
    }

    /// Extract the raw underlying number
    pub(crate) const fn into_raw(self) -> NonZeroU16 {
        self.0
    }
    /// Create a FileId from a raw NonZeroU16
    pub(crate) const fn from_raw(raw: NonZeroU16) -> Self {
        Self(raw)
    }
}

impl Debug for FileId {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let path = self
            .try_path()
            .map(|p| format!("{:?}", p))
            .unwrap_or_else(|| String::from("not interned"));
        write!(f, "FileId({id}, {path})", id = self.0.get())
    }
}