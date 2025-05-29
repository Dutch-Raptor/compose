# E0003: Expected binding after `let`

A `let` statement must be followed by a **binding pattern**, such as a variable name, before the equals sign (`=`).

This error typically occurs when a binding is omitted or mistyped, such as writing `let = ...`.

---

### Example of erroneous code

```compose error
let = 5
```

---

### Explanation

A `let` statement introduces a new binding (or destructuring pattern) and must follow the full syntax:

```text
let pattern = expression;
```

The *pattern* is required — it's what receives the value of the expression. It can be a simple name like `x`, a mutable name like `mut x`, or a destructuring pattern like `(a, b)` or `{ key }`.

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

#### ✅ Use `mut` if you intend to mutate the variable:

```compose
let mut x = 10;
x = 20;
```