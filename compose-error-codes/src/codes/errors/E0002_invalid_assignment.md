# E0002: Assignments are not allowed in expression contexts

Assignments like `x = y` are **statements**, not expressions. They cannot be used in places where an expression is expected.
- ✅ If you meant to compare, use `x == y`.
- ✅ If you meant to assign, wrap it in a block: `{ x = y }`.

---

### Example of erroneous code

```compose error
let f = (x, a) => (x = a);
```

---

### Explanation

Assignment (`x = a`) evaluates to the unit type `()` and **cannot be used where a value is expected**. This includes:

* return values in closures or functions,
* `if` and `while` conditions,
* or anywhere an actual value is needed.

This helps avoid common mistakes, such as confusing assignment with comparison:

```compose
if (a = b) { ... } // probably meant `a == b`
```

---

### Fixes

#### ✅ If you meant to compare:

```compose
let f = (x, a) => (x == a);
```

#### ✅ If you meant to assign a value:

```compose
let f = (x, a) => { x = a };
```

#### ✅ Or use `let` for rebinding the variable:

```compose
let f = (x, a) => { let x = a; x };
```