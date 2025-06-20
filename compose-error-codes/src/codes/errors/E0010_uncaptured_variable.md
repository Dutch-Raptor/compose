## E0010: Outer variables used in closure but not captured

In Compose, closures must explicitly declare any variables they use from the outer scope. These are listed in a *
*capture group** before the closure, such as `|x| () => ...`. Compose does **not** allow implicit capture.

---

### Example

```compose error
let x = 1
let y = 2
let f = () => x + y
```

✅ **Fix:**

```compose
let f = |x, y| () => x + y
```

---

### Capture modifiers

Captured variables can be annotated to control how the closure accesses them:

* `x` — capture by copy (or clone-on-write for complex types)
* `mut x` — capture as a mutable copy
* `ref x` — capture a shared reference to a `box`
* `ref mut x` — capture a mutable reference to a `box`

> 📌 Only variables bound to `box` values may be captured using `ref` or `ref mut`.
> Owned captures cannot refer to boxed values — this prevents shallow copies of references to heap-allocated data.

#### Example:

```cmps
let x = box::new(42)
let f = |ref x| () => print(*x)
```

---

This explicit capture model prevents accidental aliasing and ensures closures make their dependencies and access modes
clear.
