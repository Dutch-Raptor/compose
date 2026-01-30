# The Compose Programming Language

```rust
# compose::test::assert_eval(r#"
println("'Hello, world' from Compose!");
# "#);
```

Compose is a functionally flavoured interpreted programming language with Rust-like syntax.

Features:
- Expression focused: blocks and control flow (`if`, `match`, loops) are expressions and produce values.
- Functions as first-class citizens
- High quality diagnostics
- Variables are immutable by default
- Garbage collection
- Portable: runs on any platform that supports rust

<div class="warning">

Compose is being developed as a hobby project and is not intended for production use.

</div>

For more thorough documentation about the language, embedding the language in your application, or using the CLI check out the [docs](https://dutch-raptor.github.io/compose/compose/).

# CLI Quick start

If you want to try out Compose, the easiest way is to use the CLI.

The `compose` CLI lets you run Compose programs, explore them interactively, and inspect errors.


## Installation

Install the latest version from GitHub using [Cargo](https://doc.rust-lang.org/cargo/):

```bash
cargo install --git https://github.com/Dutch-Raptor/compose.git
```

After installation, the `compose` executable will be available on your PATH:

```bash
compose --version
```

## Usage

Create a new file called `hello.cmps` with the following contents:

```rust
println("Hello, from Compose!")
```

and run it:

```bash
compose file hello.cmps
```

This should print `Hello, from Compose!` to the console.

### Run a file

```bash
compose file examples/hello.cmps
```

### Start a REPL

```bash
compose repl
```

Load a file in the REPL:

```bash
compose repl --from examples/prelude.cmps
```

### Explain an error

```bash
compose explain E0003
```

This gives examples, explanations, and suggested fixes.