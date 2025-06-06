# E0003: Expected binding after `let`

A `let` statement must include a **binding**, like `let x = ...`.
- ✅ Add a variable name (e.g., `let x = 5;`) or a destructuring pattern (e.g., `let { a, b } = obj;`).

---

### Example of erroneous code

```compose error
let = 5
```

---

### Explanation

A `let` statement introduces a new binding and must follow this form:

```text
let pattern = expression;
```

The *pattern* is required — it determines where the value of the expression is stored. It can be:

* a variable name: `let x = 5;`
* a mutable variable: `let mut x = 5;`
* a destructuring pattern: `let (a, b) = tuple;`, `let { a } = obj;`

If you omit the pattern, as in `let = 5`, the language doesn’t know what to bind the value to.

---

### Fixes

#### ✅ Add a binding name:

```compose
let x = 5;
```

#### ✅ Use a destructuring pattern:

```compose
let { a, b } = some_object;
```