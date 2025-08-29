# Compose

> **Work in Progress** – Compose is an experimental programming language and interpreter, built as a learning project to explore language design, fault-tolerant parsing, and high-quality diagnostics.

---

## 🚧 Status

Compose is **not production-ready**. Expect breaking changes, incomplete features, and rough edges.

That said, the project already includes a working parser, interpreter, and a growing standard library.

---

## ✨ Goals and Features

* **Fault-Tolerant Parsing**
  Compose’s parser is designed to recover gracefully from syntax errors, building a complete **Concrete Syntax Tree (CST)** even when source code contains mistakes. This enables:

  * Detailed error reporting
  * Non-blocking analysis tools
  * IDE-like features in the future

* **Helpful Diagnostics**
  Compose provides **precise, user-friendly diagnostics** with labeled spans, suggestions, and contextual hints. Examples:

```rust
error[E0009]: expected a comma between the function arguments
  ┌─ main.cmps:1:14
  │
1 │ let x = foo(a c = d)
  │              ^ help: insert a comma here
  │
= help: for more information about this error, try `compose explain E0009`

error[E0002]: assignments are not allowed in expression contexts
  ┌─ main.cmps:1:17
  │
1 │ let x = foo(a c = d)
  │                 ^ assignment is not allowed here
  │
= help: if you meant to compare `c` and `d`, use `==` instead of `=`
= help: if you meant to assign to `c`, wrap the statement in a block: `{ c = ... }`
= help: or introduce a new variable with `let`: `{ let c = ... }`
= note: assignments like `c = ...` are only valid as standalone statements
= help: for more information about this error, try `compose explain E0002`

error[E0006]: expected a semicolon after a statement
  ┌─ main.cmps:1:21
  │
1 │ let x = foo(a c = d)
  │                     ^ help: insert a semicolon here
  │
= suggested fix: write a semicolon to terminate this statement:
`let x = foo(a c = d);`
= help: for more information about this error, try `compose explain E0006`

error: condition expressions require parentheses
  ┌─ main.cmps:2:12
  │
2 │ let y = if true { 1 } else { 0 }
  │            ^^^^ Expected an opening `(` before the condition
```

```rust
error[E0004]: cannot reassign to a variable declared as immutable
  ┌─ main.cmps:3:1
  │
1 │ let a;
  │     - was defined as immutable here
2 │ a = "some value";
  │ - first assignment occurred here
3 │ a = "another value";
  │ ^ cannot reassign an immutable variable
  │
= help: make the variable mutable by writing `let mut`
= note: variables are immutable by default
```

* **Interpreter Design Inspired by Typst**
  Architecture and runtime are heavily inspired by the [Typst](https://typst.app) compiler.

* **Influenced by "Writing an Interpreter in Go"**
  Thorsten Ball’s book provided foundational parsing and interpreter concepts.

---

## 🌟 Key Features of Compose

Compose is **expressive, fault-tolerant, and developer-friendly**. Highlights:

### 🧱 Statements

* **Expression Statements:** Useful for side effects.

  ```compose
  3 + 4;         // evaluates but does nothing
  println("hi"); // prints "hi"
  ```

* **Let Bindings:** Immutable by default; mutable with `let mut`.

  ```compose
  let x = 42;
  let mut y = 5;
  y = 6; // allowed
  ```

* **Assignments:** Must target existing variables. Compose provides clear errors for undeclared or immutable targets.

---

### ✨ Expressions

Compose treats almost everything as an expression, making code **flexible and composable**.

* **Literals & Variables:**

  ```compose
  42; true; "hello"; x;
  ```

* **Arithmetic & Logical Operators:**

  ```compose
  1 + 2 * 3;
  x > 5 && y != 0;
  ```

* **Functions & Closures:** First-class values, can be assigned, passed, or returned.

  ```compose
  let add = { x, y => x + y; };
  add(1, 2); // returns 3

  let make_adder = { x => { y => x + y; } };
  let add_one = make_adder(1);
  add_one(2); // returns 3
  ```

* **Blocks:** Return the value of the last expression.

  ```compose
  let result = { let y = 3; y + 1; }; // 4
  ```

* **Conditionals:** `if` expressions produce a value.

  ```compose
  let x = if a > 10 { "big" } else { "small" };
  ```

* **Loops:** `while` and `for` can return a value using `break`.

  ```compose
  let result = {
      let mut i = 0;
      while true {
          if i == 5 { break i * 2; }
          i += 1;
      }
  }; // 10
  ```

---

### 🗂 Expression Summary

| Type               | Returns a Value? | Example                            |
| ------------------ | ---------------- | ---------------------------------- |
| Literal            | ✅                | `42`, `"hi"`                       |
| Variable Reference | ✅                | `x`                                |
| Arithmetic         | ✅                | `1 + 2 * 3`                        |
| Function Call      | ✅                | `add(1, 2)`                        |
| Block `{ ... }`    | ✅                | `{ let x = 2; x + 1 }`             |
| `if` / `else`      | ✅                | `if x > 0 { "yes" } else { "no" }` |
| `while` / `for`    | ✅ via `break`    | `while cond { break 5; }`          |
| Closure            | ✅                | `{ x => x + 1 }`                   |

> **Philosophy:** if it does something, it probably returns something too.

---

## 🔍 Why Compose Exists

Compose is a **learning exercise**:

* Explore **language design**, parsing, and interpreter internals.
* Build **robust developer tools** with IDE-quality feedback.
* Experiment with:

  * Error recovery that doesn’t compromise correctness
  * Clear diagnostics with fix suggestions
  * Garbage Collection
  * Compile-time checked documentation

---

## 🛠️ Current State

* ✅ Hand-written fault-tolerant parser (CST-based)
* ✅ Interpreter with value/reference model
* ✅ Support for arrays, maps, functions, closures, boxed values
* ✅ Support for cyclic data structures in runtime
* ✅ Garbage collector (mark-and-sweep)
* 🚧 Import system (single-evaluation, no cycles, partial)
* 🚧 Iterators (some combinators implemented)
* 🚧 Standard library growth
* 🚧 Error message improvements and linter-like suggestions

---

## 📚 References & Inspirations

* [Typst](https://typst.app): Clean interpreter & VM design
* *Writing an Interpreter in Go* by Thorsten Ball
* [codespan-reporting](http://github.com/brendanzab/codespan) and [Spade Lang fork](https://gitlab.com/spade-lang/codespan)

---

## 🔮 Roadmap Ideas

* IDE integration via LSP
* More expressive types and pattern matching
* Inline documentation and REPL
* Educational visualizations for debugging

---

## 🤝 Contributing

Primarily a **personal learning project**, but feedback and discussions are welcome.

---

## License

Compose is licensed under the [Apache License 2.0](LICENSE).
You are free to use, modify, and distribute Compose in personal or commercial projects, provided you include the license text and notices.