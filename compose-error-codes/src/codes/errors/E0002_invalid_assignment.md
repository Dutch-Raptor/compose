# E0002: Assignments are not allowed in expression contexts

Assignment expressions like `x = y` are **not valid in expression positions**. Assignment is a **statement**, not an
expression — it does not return a value, and therefore cannot appear where a value is expected.

This restriction exists to prevent common mistakes, such as confusing assignment (`=`) with comparison (`==`).

#### Example of erroneous code:

```compose error
let f = (x, a) => (x = a);
```

### Explanation

Assignment (`x = a`) has the unit type `()`, and is not intended to produce a value. Therefore, it cannot be used:

* as a return value in closures or functions,
* as a condition in `if` or `while` expressions,
* or in any other context where an expression with a meaningful value is required.

This helps catch bugs like:

```compose
if (a = b) { ... } // probably meant `a == b`
```

### Fixes

Depending on what you intended, there are different ways to correct the code:

#### ✅ If you meant to compare:

```compose
let f = (x, a) => (x == a);
```

#### ✅ If you meant to assign:

```compose
let f = (x, a) => { x = a };
```

#### ✅ Or use `let` for rebinding:

```compose okay
let f = (x, a) => { let x = a; x };
```
