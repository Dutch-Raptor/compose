## E0311: Match arm patterns bind different variables

### Error

```ignore
patterns within a match arm have differing bindings
```

---

### Meaning

All patterns joined with `|` in a `match` arm share the same body.
That body must be valid for **every** pattern.

Therefore, **each pattern must bind the same variables**.

---

### Example

```compose error(E0311)
match value {
    [0, a] | [1, b] => a + b
}
```

```output error(E0311)
```

This is invalid:

* `[0, a]` binds `a`
* `[1, b]` binds `b`
* the body requires both

No execution binds both variables.

---

### Why this is rejected

A `|`-joined arm behaves like multiple patterns with a single body:

```compose
p1 | p2 => body
```

Since `body` is shared, it may only reference variables bound by **all** patterns.

---

### Fixes

**Use the same bindings:**

```compose
# let value = [0, 1];
match value {
    [0, x] | [1, x] => x
}
```

**Or split the arms:**

```compose
#let value = [0, 1];
match value {
    [0, a] => a,
    [1, b] => b,
}
```

---

### Summary

* `|`-joined patterns must bind identical variables
* the arm body must work for every pattern
* rename bindings or split the arm to resolve the error
