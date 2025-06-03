## W0003: Unnecessary parentheses in `for` expression

In Compose, `for` loop headers do not require parentheses. While allowed in some languages, parentheses in `for` loops are redundant in Compose and should be omitted for clarity and stylistic consistency.

---

### Example

```compose warning
for (x in items) {
    print(x)
}
```

✅ **Fix:**

```compose
for x in items {
    print(x)
}
```

---

This warning is issued to encourage idiomatic Compose syntax and reduce visual clutter. It applies to `for`, `while`, and `if` conditions where parentheses are not required.
