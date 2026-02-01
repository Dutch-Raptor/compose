/*!
# Embedding Compose

To learn about embedding Compose in your application, we'll create a CLI similar to the one
provided in compose_cli.

It will support executing a Compose source file, loading other source files, reading from stdin, and printing to stdout.

## Creating a World

We want to create a [`World`](crate::World) implementation backed by the file system working from the current directory.

```rust
use std::collections::HashMap;
use std::fs;
use std::io::{Read, Write};
use std::path::{PathBuf, Path};
use std::sync::Mutex;
use compose::library::diag::{FileError, FileResult};
use compose::library::{Library, World};
use compose::syntax::{FileId, Source};

struct SystemWorld {
    entrypoint: FileId,
    // Since the world trait works through a shared reference, we need to add a mutex
    // to allow mutation through a shared reference.
    sources: Mutex<HashMap<FileId, Source>>,
    library: Library,
    // Keep track of the root directory so that we can resolve relative paths.
    root: PathBuf,
}

impl SystemWorld {
    fn from_file(path: impl AsRef<Path>) -> FileResult<Self> {
        let path = path.as_ref();
        let root = path.parent().unwrap().to_path_buf()
            .canonicalize()
            .map_err(|e| FileError::from_io(e, path))?;

        let entrypoint = FileId::new(path);
        let text = fs::read_to_string(path)
            .map_err(|e| FileError::from_io(e, path))?;
        let source = Source::new(entrypoint, text);

        let mut sources = HashMap::new();
        sources.insert(source.id(), source);

        Ok(Self {
            entrypoint,
            sources: Mutex::new(sources),
            library: Library::default(),
            root,
        })
    }

    fn add_source(&self, source: Source) {
        self.sources.lock().unwrap().insert(source.id(), source);
    }

    fn get_or_read_from_disk(&self, file_id: FileId) -> FileResult<Source> {
        let mut sources = self.sources.lock().unwrap();
        if let Some(source) = sources.get(&file_id) {
            // Source uses reference counting and is inexpensive to clone, so we can return it directly.
            return Ok(source.clone());
        }

        let path = file_id.path();

        let file_contents = fs::read_to_string(path.as_path())
            .map_err(|e| FileError::from_io(e, path.as_path()))?;

        let source = Source::new(file_id, file_contents);

        sources.insert(source.id(), source.clone());

        Ok(source)
    }
}

impl World for SystemWorld {
    fn entry_point(&self) -> FileId {
        self.entrypoint
    }

    fn source(&self, file_id: FileId) -> FileResult<Source> {
        self.get_or_read_from_disk(file_id)
    }

    fn library(&self) -> &Library {
        &self.library
    }

    fn write(&self, f: &mut dyn FnMut(&mut dyn Write) -> std::io::Result<()>) -> std::io::Result<()> {
        f(&mut std::io::stdout())
    }

    fn read(&self, f: &mut dyn FnMut(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()> {
        f(&mut std::io::stdin())
    }

    fn name(&self, id: FileId) -> String {
        // return the path relative to the root, if within the root.
        id.path()
            .0
            .strip_prefix(&self.root)
            .unwrap_or(&id.path().0)
            .display()
            .to_string()
    }
}
```

That was most of the work! All that's left to do is reading the
file path from the command line arguments, creating a world, creating a vm, and evaluating the file.

```rust
# use std::collections::HashMap;
# use std::fs;
# use std::io::{Read, Write};
use std::path::PathBuf;
# use std::path::Path;
# use std::sync::Mutex;
# use compose::library::diag::{FileError, FileResult};
# use compose::library::{Library, World};
# use compose::syntax::{FileId, Source};
#
# struct SystemWorld {
#     entrypoint: FileId,
#     // Since the world trait works through a shared reference, we need to add a mutex
#     // to allow mutation through a shared reference.
#     sources: Mutex<HashMap<FileId, Source>>,
#     library: Library,
#     // Keep track of the root directory so that we can resolve relative paths.
#     root: PathBuf,
# }
#
# impl SystemWorld {
#     fn from_file(path: impl AsRef<Path>) -> FileResult<Self> {
#         let path = path.as_ref();
#         let root = path.parent().unwrap().to_path_buf()
#             .canonicalize()
#             .map_err(|e| FileError::from_io(e, &path))?;
#
#         let entrypoint = FileId::new(path);
#         let text = fs::read_to_string(path)
#             .map_err(|e| FileError::from_io(e, &path))?;
#         let source = Source::new(entrypoint, text);
#
#         let mut sources = HashMap::new();
#         sources.insert(source.id(), source);
#
#         Ok(Self {
#             entrypoint,
#             sources: Mutex::new(sources),
#             library: Library::default(),
#             root,
#         })
#     }
#
#     fn add_source(&self, source: Source) {
#         self.sources.lock().unwrap().insert(source.id(), source);
#     }
#
#     fn get_or_read_from_disk(&self, file_id: FileId) -> FileResult<Source> {
#         let mut sources = self.sources.lock().unwrap();
#         if let Some(source) = sources.get(&file_id) {
#             // Source uses reference counting and is inexpensive to clone, so we can return it directly.
#             return Ok(source.clone());
#         }
#
#         let path = file_id.path();
#
#         let file_contents = fs::read_to_string(path.as_path())
#             .map_err(|e| FileError::from_io(e, path.as_path()))?;
#
#         let source = Source::new(file_id, file_contents);
#
#         sources.insert(source.id(), source.clone());
#
#         Ok(source)
#     }
# }
#
# impl World for SystemWorld {
#     fn entry_point(&self) -> FileId {
#         self.entrypoint
#     }
#
#     fn source(&self, file_id: FileId) -> FileResult<Source> {
#         self.get_or_read_from_disk(file_id)
#     }
#
#     fn library(&self) -> &Library {
#         &self.library
#     }
#
#     fn write(&self, f: &mut dyn FnMut(&mut dyn Write) -> std::io::Result<()>) -> std::io::Result<()> {
#         f(&mut std::io::stdout())
#     }
#
#     fn read(&self, f: &mut dyn FnMut(&mut dyn Read) -> std::io::Result<()>) -> std::io::Result<()> {
#         f(&mut std::io::stdin())
#     }
#
#     fn name(&self, id: FileId) -> String {
#         // return the path relative to the root, if within the root.
#         id.path()
#             .0
#             .strip_prefix(&self.root)
#             .unwrap_or(&id.path().0)
#             .display()
#             .to_string()
#     }
# }


use std::process::exit;
use compose_eval::{eval, Machine};
use compose_library::diag::print_diagnostics;

fn file(file_path: impl AsRef<Path>) {
    let path = file_path.as_ref();
    let world = SystemWorld::from_file(&path).expect("Failed to load entrypoint file");
    let mut vm = Machine::new(&world);

    let result = eval(&mut vm);

    if let Err(err) = result.value {
        print_diagnostics(&world, &err, &result.warnings, false).expect("failed to print diagnostics");
        exit(1);
    }

    // print warnings
    print_diagnostics(&world, &[], &result.warnings, false).expect("failed to print diagnostics");
}
```
*/

