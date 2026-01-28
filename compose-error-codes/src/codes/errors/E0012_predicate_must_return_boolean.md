## E0012: Predicate functions must return a boolean value

Methods like `all`, `filter`, `find`, and similar take a **predicate function**: a function that must return either `true` or `false`. This error occurs when the predicate returns a different type, such as an `int` or `string`.

---

### Example

```compose error(E0012)
(1..5).iter().any { x => 3 };
```

#### Error:

```output error(E0012)
```

✅ **Fix:**

```compose
(1..5).iter().any { x => x == 3 };
```

---

This rule ensures that the method can correctly decide whether each element satisfies the condition. Non-boolean values are not treated as truthy or falsy, instead you must write an explicit comparison.

If you intended to filter or match by value, use a comparison like `x == value`, `x > threshold`, or a boolean-returning expression.
