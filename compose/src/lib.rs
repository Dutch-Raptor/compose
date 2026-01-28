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

**This language is still in development, and APIs are subject to change. Developed as a learning
project, the language is not intended for production use.**

To learn about the language, see the [language] module.

To learn about the implementation, see the [implementation] docs.
*/
pub mod language;
pub mod implementation;

pub use compose_eval::{eval, eval_range, test};

#[doc(hidden)]
pub use compose_syntax as syntax;
#[doc(hidden)]
pub use compose_eval as eval;
#[doc(hidden)]
pub use compose_library as library;
