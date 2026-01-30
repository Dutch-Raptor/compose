/*!
# The Compose Programming Language

```rust
# compose::test::assert_eval(r#"
println("'Hello, world' from Compose!");
# "#);
```

A functional flavoured interpreted programming language with rust-like syntax.

Features:
- Expression focused: blocks and control flow are expressed as expressions.
- Functions as first class citizens
- High quality diagnostics
- Variables are immutable by default
- Garbage collection
- Portable: runs on any platform that supports rust

<div class="warning">

Compose is being developed as a hobby project and is not intended for production use.

</div>

To learn about the language, see the [language] module.

To learn about how compose is implemented internally, see the [implementation] docs.

# Using this crate

Add compose to your dependencies:

```toml
[dependencies]
compose = { git = "https://github.com/Dutch-Raptor/compose.git" }
```

To make Compose portable, all interaction with the outside world goes through the [`World`] trait.

Let's start by implementing that.

```rust
use compose::{
    World,
    eval_source,
    eval::EvalConfig,
    eval::Machine,
    library::Library,
    library::repr::Repr,
    library::diag::{FileError, FileResult, print_diagnostics},
    syntax::FileId,
    syntax::Source,
    library::{Value}
};
use std::collections::HashMap;
use std::io::{Read, Write};

struct ExampleWorld {
    /// The entrypoint
    main: FileId,
    /// Any other sources that have been loaded
    sources: HashMap<FileId, Source>,
    library: Library,
}

impl World for ExampleWorld {
    fn entry_point(&self) -> FileId {
        self.main
    }

    fn source(&self, file_id: FileId) -> FileResult<Source> {
        match self.sources.get(&file_id).cloned() {
            Some(source) => Ok(source),
            None => Err(FileError::NotFound(file_id.path().to_path_buf())),
        }
    }

    fn library(&self) -> &Library {
        &self.library
    }

    fn write(
        &self,
        f: &mut dyn FnMut(&mut dyn Write) -> std::io::Result<()>,
    ) -> std::io::Result<()> {
        let mut stdout_lock = std::io::stdout().lock();
        f(&mut stdout_lock)
    }

    fn read(
        &self,
        f: &mut dyn FnMut(&mut dyn Read) -> std::io::Result<()>,
    ) -> std::io::Result<()> {
        let mut stdin_lock = std::io::stdin().lock();
        f(&mut stdin_lock)
    }
}

impl ExampleWorld {
    fn new(main_source: Source) -> Self {
        Self {
            main: main_source.id(),
            sources: {
                let mut map = HashMap::new();
                map.insert(main_source.id(), main_source);
                map
            },
            library: Library::empty(),
        }
    }
}

fn main() {
    // Define/load the source to be interpreted
    let source_text = r#"
        println("Hello from Compose");
        2 + 3 // the value of the last expression is returned
    "#;

    let file_name = "main.cmps";
    let source = Source::from_string(file_name, source_text);

    // Define the world that contains the source
    let world = ExampleWorld::new(source.clone());
    // Create a VM with access to that world.
    let mut vm = Machine::new(&world);

    // evaluate the source
    let warned_result = eval_source(&source, &mut vm, &EvalConfig::default());


    print_diagnostics(&world, &[], &warned_result.warnings, false)
        .expect("Failed to print diagnostics");

    let result = match warned_result.value {
        Ok(value) => value,
        Err(errors) => {
            print_diagnostics(&world, &errors, &[], false)
                .expect("Failed to print diagnostics");
            return;
        }
    };

    println!("{}", result.repr(&vm));

    assert_eq!(Value::Int(5), result);
}
```
*/
pub mod implementation;
pub mod language;

pub use compose_eval::{eval_source, eval_source_range, test};
pub use compose_library::{SourceResult, World, diag::SourceDiagnostic, diag::Warned};

#[doc(hidden)]
pub use compose_eval as eval;
#[doc(hidden)]
pub use compose_library as library;
#[doc(hidden)]
pub use compose_syntax as syntax;
