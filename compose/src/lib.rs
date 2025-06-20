/*!
# The Compose Programming Language

```rust
# compose::test::assert_eval(r#"
println("Hello, world! From Compose!");
# "#);
```

**Compose** is a lightweight, expression-oriented programming language designed to be clear, predictable, and structurally robust. Inspired by modern language design principles, Compose aims to offer a seamless blend of expressiveness and simplicity—without the overhead of complex ownership models or verbose syntax.

Whether you're scripting quick logic or building more sophisticated constructs, Compose is designed to guide you gently, offering:

* **Clear syntax**: Minimal punctuation and consistent structure help you focus on *what* you're expressing, not *how* to express it.
* **Safe references**: Compose uses a `box` system to handle heap-allocated data with clarity and safety. References (`ref`, `ref mut`) are only allowed to refer to boxed values, which helps avoid subtle lifetime bugs.
* **No move semantics**: All values are either copyable or clone-on-write, simplifying reasoning about variable behavior and reducing surprises.
* **Robust error reporting**: Compose comes with detailed, context-aware error messages that point you to the real problem—often with suggestions to fix it.
* **A strong, consistent AST and CST**: Behind the scenes, Compose uses a resilient parser and a fault-tolerant concrete syntax tree, making tooling and analysis more reliable and powerful.
* **First-class closures**: Functions and closures are fully supported, and closures can capture boxed variables by reference, safely enabling functional patterns.

To learn more, check out the [Introduction](docs::C1_Introduction) section.

## Getting Started

- [Docs](docs)
    - [Introduction](docs::C1_Introduction)

*/
pub mod docs;

pub use compose_eval::{eval, eval_range, test};

pub use compose_error_codes as error_codes;
