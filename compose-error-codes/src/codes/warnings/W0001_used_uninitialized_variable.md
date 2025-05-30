# W0001: Use of a variable before it has been initialized

A variable declared with `let` but not initialized defaults to `()`.
- ✅ Assign a value when declaring (e.g., `let a = 5;`)
- ✅ Or assign a value before first use (e.g., `let a; a = 5; a`)


---

### Example of problematic code

```compose warning
let a;
a
```

---

### Explanation

When you declare a variable with `let` but do **not** give it an initial value, Compose defaults it to the unit value `()` (similar to `void` in other languages).

While this is valid syntax, it often indicates a mistake — such as assuming the variable has a meaningful default or forgetting to initialize it before use.

This warning helps prevent subtle logic bugs or no-op evaluations.

---

### Fixes

#### ✅ Initialize the variable during declaration:

```compose
let a = 42;
a
```

#### ✅ Assign a value before using the variable:

```compose
let a;
a = 10;
a
```

#### ✅ If you meant to use the unit value, make it explicit:

```compose
let a = ();
a // evaluates to ()
```

---

### Why this matters

Uninitialized variables default to `()`, which is often unintentional. For example:

```compose warning
let total;
println("Total is", total);
```

This will print `"Total is ()"` — which likely isn't what you meant.