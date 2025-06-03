## E0006: A semicolon or newline is required after each statement.

In Compose, each statement must be followed by either a semicolon (`;`) or a newline. This makes statement boundaries unambiguous and consistent.

---

### Example

```compose error
let x = 1 let y = 2
```

✅ **Fix:**

```compose
let x = 1; let y = 2;
```

or

```compose
let x = 1
let y = 2
```

---

This rule applies to all statements, including variable declarations, expressions in blocks, and control-flow constructs. Use semicolons to write multiple statements on one line, or place each on a new line.
