## E0010: Unbound variable

This error occurs when you try to use a variable that has not been declared in the current scope.

### Example:

```compose error
y = 4; // ❌ Error: `y` is unbound
```

Here, the variable `y` is used as if it already exists, but it hasn’t been introduced with a `let` binding or passed in as a parameter.

### 💡 Solution:

Introduce the variable first using `let`:

```compose okay
let mut y = 2; // ✅ now `y` is declared
y = 4;
```

Or make sure the variable is in scope when used.

---

### 🧠 Why this happens

Compose enforces explicit bindings to avoid mistakes like typos or accidental use of undeclared variables. This helps catch bugs early and keeps code clear and predictable.