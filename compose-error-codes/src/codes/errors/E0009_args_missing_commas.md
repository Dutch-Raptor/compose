## E0009: Function arguments must be separated by commas

In Compose, arguments passed to a function must be separated by commas. This error occurs when multiple arguments are listed without a comma, making the argument list syntactically invalid.

---

### Example

```compose error
add(x y z)
```

✅ **Fix:**

```compose
add(x, y, z)
```

---

This rule applies to all function calls. Always use commas to clearly separate each argument in the list.
