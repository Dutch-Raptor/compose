# W0001: Use of a variable before it has been initialized

This warning is emitted when you use a variable that was declared with `let` but not given an initial value.

---

### Example of problematic code

```compose warning
let a;
a
```

---

### Explanation

When you declare a variable without assigning it a value, it defaults to the **unit value `()`**, similar to `void` in other languages.

While this may be valid syntax, it’s often a sign of a mistake — for example, forgetting to assign a value or assuming the variable has a meaningful default.

---

### Fixes

#### ✅ Initialize the variable at declaration time:

```compose
let a = 42;
a
```

#### ✅ If you intend to assign it later, do so before using it:

```compose
let a;
a = 10;
a
```

#### ✅ If you truly mean to use the unit value:

```compose
let a = ();
a // evaluates to ()
```

---

### Why this matters

Using an uninitialized variable results in the value `()`, which is not very useful if unintended. This warning helps you catch cases where a variable was declared but never properly initialized.

It’s particularly useful in catching logic bugs like:

```compose warning
let total;
print("Total is", total);
```