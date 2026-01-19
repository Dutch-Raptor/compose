## Error E0303: missing key in map pattern

### What this means

This error occurs when a map destructuring pattern **requires a key that is not present** in the map being destructured.

In Compose, map patterns list the keys that must exist.
If any required key is missing from the value, destructuring fails.

---

### Example that triggers this error

```compose
let { a, b } = { a: 1 };
```

The pattern requires the keys `a` and `b`, but the map only contains `a`.
Since `b` is not present, the pattern cannot be matched.

---

### Why this is an error

Map destructuring is **strict**:

* Every key named in the pattern must exist in the map
* Missing keys would otherwise result in uninitialized bindings

Rather than implicitly assigning a default value or ignoring missing keys, Compose reports an error to ensure correctness.

---

### How to fix it

You have several options, depending on your intent.

#### Provide the missing key

```compose
let { a, b } = { a: 1, b: 2 };
```

#### Remove the key from the pattern

```compose
let { a } = { a: 1 };
```

#### Conditionally match the map

If the map may or may not contain the key, use a **pattern test** instead of direct destructuring.

Patterns can be used in both `match` expressions **and** `is` expressions:

```compose
let map = { a: 1 };

if map is { a, b } {
    println(a, b);
}
```

The `is` expression only returns true if the pattern matches.
Inside the body of the `if` expression, all pattern bindings (`a` and `b`) are guaranteed to exist.

The same pattern can also be used in a `match` expression:

```compose
match map {
    { a, b } => println(a, b),
    _ => (),
}
```

### Summary

* Keys listed in a map pattern are required
* Direct destructuring is unconditional
* Use `if … is` or `match` to conditionally match maps
* This error ensures missing data is handled explicitly
