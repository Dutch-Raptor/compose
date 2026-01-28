## E0302: map destructuring does not cover all keys

### What this means

This error occurs when a map destructuring pattern does not account for **every key** in the map being destructured.

In Compose, map destructuring is **exhaustive** by default:
if a map contains keys that are not explicitly destructured, the pattern is rejected.

---

### Example that triggers this error

```compose error(E0302)
let { a } = { a: 1, b: 2 };
```

The map contains the keys `a` and `b`, but the pattern only destructures `a`.
Since `b` is not handled, this results in an error.

---

### Why this is an error

Allowing partial map destructuring without being explicit would make it unclear whether:

* extra keys are intentionally ignored, or
* a key was forgotten by mistake

To avoid silent bugs, Compose requires you to **explicitly choose** what happens to the remaining keys.

---

### How to fix it

You have two options.

#### Ignore remaining keys

Use `..` to explicitly discard any keys not listed in the pattern:

```compose
let { a, .. } = { a: 1, b: 2 };
```

#### Capture remaining keys

Use `..name` to bind the remaining keys into a new map:

```compose
let { a, ..rest } = { a: 1, b: 2 };
```

Here, `rest` will be a map containing `{ b: 2 }`.

---

### Summary

* Map destructuring in Compose is exhaustive by default
* All keys must be handled explicitly
* Use `..` to ignore remaining keys
* Use `..name` to capture remaining keys into a map

This design favors clarity and prevents accidental data loss.