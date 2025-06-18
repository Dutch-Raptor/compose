## E0006: A semicolon is required after each statement.

In Compose, each statement must be followed by a semicolon (`;`). This makes statement boundaries unambiguous and
consistent.

---

### Example

```compose error
let x = 1 let y = 2
```

✅ **Fix:**

```compose
let x = 1; let y = 2;
```

---

This rule applies to all statements, including variable declarations, expressions in blocks, and control-flow
constructs.
