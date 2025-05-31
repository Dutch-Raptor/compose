## W0002: Parentheses around a condition are not required and should be removed.

In Compose, `if` and `while` conditions do not need to be wrapped in parentheses. While this is valid syntax, it is unnecessary and discouraged.

---

### Example

```compose warning
if (true) {
    println("it was true")
}
```

✅ **Fix:**

```compose
if true {
    // ...
}
```

---

This applies to any condition expression in control flow constructs such as `if` and `while`.

Parentheses are still allowed when used for grouping or precedence control, but they should not be used around condition expressions by default.