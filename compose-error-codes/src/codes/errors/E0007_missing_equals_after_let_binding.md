## E0007: A `let` binding must be followed by `=` or terminated with a semicolon or newline.

In Compose, a `let` statement introduces a new variable binding. After the binding pattern (e.g., `count`), the parser expects either:

- an initializer with `=`,
- or a clear statement terminator: a semicolon (`;`) or a newline.

---

### Example

```compose error
let count 42
```

✅ **Fix:**

```compose
let count = 42
```

or

```compose
let count; 42
```

or

```compose
let count
42
```

---

This requirement avoids ambiguity between incomplete bindings and separate expressions. Use `=` to initialize, or a semicolon or newline to end the declaration.
