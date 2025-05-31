## E0005: `if` expressions must use braces to delimit their body.

In Compose, the body of an `if` expression must be enclosed in `{}`. Unlike some languages that allow single-line, brace-less `if` statements, Compose requires explicit block delimiters to avoid ambiguity and enforce consistent syntax.

---

### Example

```compose error
if true
    let a = true;
```

✅ **Fix:**

```compose
if true {
    let a = true;
}
```

---

This rule applies to all `if`, `else if`, and `else` blocks used as expressions or statements.

Enforcing block braces ensures a consistent syntax model and avoids subtle bugs related to indentation or accidental semicolon usage.
