/*!
# The Compose Programming Language

```rust
# compose::test::assert_eval(r#"
println("'Hello, world' from Compose!");
# "#);
```

Compose is a functional-flavoured, interpreted programming language with Rust-like syntax.

Features:
- Expression focused: blocks and control flow (`if`, `match`, loops) are expressions and produce values.
- Functions as first-class citizens
- High quality diagnostics
- Variables are immutable by default
- Garbage collection
- Portable: runs on any platform that supports Rust

<div class="warning">

Compose is being developed as a hobby project and is not intended for production use.

</div>

## Documentation overview

- The [`language`] module documents Compose syntax and semantics.
- The [`implementation`] module documents the internals of the language implementation.
- The [`embedding`] module documents how to embed Compose in your application.
- The [`cli`] module documents the CLI.

## Using this crate

> Below follows a minimal example of embedding Compose in your application. If you want to take a deeper dive, check out the [embedding] documentation.

Add Compose to your dependencies:

```toml
[dependencies]
compose = { git = "https://github.com/Dutch-Raptor/compose.git" }
```

### Creating a [`World`]

Compose is designed to be **embedded** in your application. All interactions with the outside world (like reading files, printing to the console, etc.) go through the [`World`] trait. This allows running Compose in a CLI, in tests, or embedded in another application.

A [`World`] should provide:
- The entrypoint source of the program.
- A way to access source files.
- Standard input and output.
- The standard library.

Source files could be loaded from disk, stored in memory, or come from any other source.
Standard input and output can be virtualised. The standard library can be customised to provide additional functionality.

In this minimal example, we define a [`World`] that contains a single source file.

```rust
use compose::{
    World,
    library::Library,
    library::diag::{FileError, FileResult},
    syntax::{FileId, Source},
};
use std::collections::HashMap;
use std::io::{Read, Write};

struct ExampleWorld {
    main: Source,
    library: Library,
}

impl World for ExampleWorld {
    fn entry_point(&self) -> FileId {
        self.main.id()
    }

    // This example world only supports a single source, so that is the only source it can return.
    fn source(&self, file_id: FileId) -> FileResult<Source> {
        if file_id == self.main.id() {
            Ok(self.main.clone())
        } else {
            Err(FileError::NotFound(file_id.path().as_path().clone()))
        }
    }

    fn library(&self) -> &Library {
        &self.library
    }

    // Write to stdout
    fn write(
        &self,
        f: &mut dyn FnMut(&mut dyn Write) -> std::io::Result<()>,
    ) -> std::io::Result<()> {
        let mut stdout_lock = std::io::stdout().lock();
        f(&mut stdout_lock)
    }

    // read from stdin
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
            main: main_source,
            library: Library::default(),
        }
    }
}
```

### Loading source code

Compose source code is represented by the [`Source`](compose_syntax::Source) type.

```rust
use compose::syntax::Source;

let source_text = r#"
    println("Hello from Compose");
    2 + 3 // the value of the last expression is returned
"#;

let source = Source::from_string("main.cmps", source_text);
```

### Evaluating source code

```rust
use compose::{
    evaluation::{Machine, eval},
    library::diag::print_diagnostics,
    library::repr::Repr,
    library::Value,
    # syntax::Source,
    # syntax::FileId,
    # World,
    # library::Library,
    # library::diag::{FileError, FileResult},
};
use std::process::exit;
# use std::collections::HashMap;
# use std::io::{Read, Write};

# struct ExampleWorld {
#     main: Source,
#     library: Library,
# }
#
# impl World for ExampleWorld {
#     fn entry_point(&self) -> FileId {
#         self.main.id()
#     }
#
#     // This example world only supports a single, so that is the only source it can return.
#     fn source(&self, file_id: FileId) -> FileResult<Source> {
#         if file_id == self.main.id() {
#             Ok(self.main.clone())
#         } else {
#             Err(FileError::NotFound(file_id.path().as_path().clone()))
#         }
#     }
#
#     fn library(&self) -> &Library {
#         &self.library
#     }
#
#     // Write to stdout
#     fn write(
#         &self,
#         f: &mut dyn FnMut(&mut dyn Write) -> std::io::Result<()>,
#     ) -> std::io::Result<()> {
#         let mut stdout_lock = std::io::stdout().lock();
#         f(&mut stdout_lock)
#     }
#
#     // read from stdin
#     fn read(
#         &self,
#         f: &mut dyn FnMut(&mut dyn Read) -> std::io::Result<()>,
#     ) -> std::io::Result<()> {
#         let mut stdin_lock = std::io::stdin().lock();
#         f(&mut stdin_lock)
#     }
# }
#
# impl ExampleWorld {
#     fn new(main_source: Source) -> Self {
#         Self {
#             main: main_source,
#             library: Library::default(),
#         }
#     }
# }

# fn main() {
# let source = Source::from_string("main.cmps", "2 + 3");
let world = ExampleWorld::new(source.clone());
let mut vm = Machine::new(&world);

// `eval` evaluates the entrypoint source provided by the `world`.
let result = eval(&mut vm);

// Print any warnings
print_diagnostics(&world, &[], &result.warnings, false).unwrap();

let value = match result.value {
    Ok(value) => value,
    Err(errors) => {
        print_diagnostics(&world, &errors, &[], false).unwrap();
        exit(1);
    }
};

// Print the resulting value
println!("{}", value.repr(&vm));
assert_eq!(Value::Int(5), value);
# }

```

To learn more about embedding Compose in your application, check out the [embedding] documentation.
*/
pub mod implementation;
pub mod language;
pub mod embedding;
pub mod cli;

pub use compose_eval::{eval_source, eval_source_range, test};
pub use compose_library::{SourceResult, World, diag::SourceDiagnostic, diag::Warned};

#[doc(hidden)]
pub use compose_eval as evaluation;
#[doc(hidden)]
pub use compose_library as library;
#[doc(hidden)]
pub use compose_syntax as syntax;
