# E0004: Cannot reassign to a variable declared as immutable

Variables declared with `let` are **immutable by default**.

- ✅ Write `let mut` if you want to change their value after initialization.

---

### Example of erroneous code

```compose error
let a
a = 2
a = 3
```

Or more simply:

```compose error
let b = 4
b = 5
```

---

### Explanation

In Compose, variables are **immutable by default**. This means you can assign to them once, but attempting to reassign will produce an error.

To allow a variable to change over time, declare it as **mutable** using `mut`:

```compose
let mut counter = 0;
counter = counter + 1;
```

This design prevents accidental changes and helps promote safer and more predictable code.

---

### Fixes

#### ✅ Make the variable mutable:

```compose
let mut a = 2;
a = 3;
```

#### ✅ Or, use shadowing (create a new variable):

```compose
let a = 2;
// ...
let a = 3; // this creates a new `a`
```

Shadowing can be a good alternative to mutation, especially in functional-style code.