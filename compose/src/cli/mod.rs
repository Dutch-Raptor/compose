/*!
# Compose CLI

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

```text
println("Hello, from Compose!")
```

and run it:

```bash
compose file hello.cmps
```

This should print `Hello, from Compose!` to the console.

### Run a file

```bash
compose file hello.cmps
```

### Start a REPL

```bash
compose repl
```

Load a file in the REPL:

```bash
compose repl --from prelude.cmps
```

### Explain an error

```bash
compose explain E0003
```

This gives examples, explanations, and suggested fixes.
*/