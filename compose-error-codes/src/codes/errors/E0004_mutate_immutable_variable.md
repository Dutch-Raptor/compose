# E0004: Cannot reassign to a variable declared as immutable

By default, variables declared with `let` are **immutable** — they can be assigned once, but not changed later. Attempting to assign a new value to an immutable variable causes this error.

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

In Compose, variables are **immutable by default**. This means you can assign to them once, and any attempt to reassign will result in an error.

If you need a variable to hold a changing value — such as inside a loop, or during incremental updates — you must explicitly mark it as `mut`:

```compose
let mut counter = 0;
counter = counter + 1;
```

This helps prevent accidental mutations, encourages more predictable code, and supports functional-style programming patterns.

---

### Fixes

#### ✅ Make the variable mutable:

```compose
let mut a = 2;
a = 3;
```

#### ✅ Or, if mutation isn’t needed, use a new variable:

```compose
let a = 2;
// ...
let a = 3; // shadows the previous `a`
```

This avoids mutation while still reusing the name, which can be appropriate in more functional-style code.

---

### Common mistakes

#### 🚫 Declaring a variable but reassigning without `mut`:

```compose
let value = 10;
value = 20; // error
```

✅ Fix:

```compose
let mut value = 10;
value = 20;
```