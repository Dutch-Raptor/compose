# E0001: Unclosed delimiter

This error occurs when the parser encounters an opening delimiter like `(`, `{`, or `[` that isn’t properly closed. Compose requires all delimiters to be **balanced and properly nested**.

Unclosed delimiters can happen in several ways: forgetting a closing character, accidentally mismatching different kinds of delimiters, or reaching the end of the file before closing.

---

#### Example 1: Missing closing parenthesis

```compose error
(unclosed_paren == 2
let other_statement
```

The opening `(` is never closed before the next statement begins.

✅ **Fix:**

```compose
(unclosed_paren == 2)
let other_statement
```

---

#### Example 2: Unexpected closing delimiter

```compose error
{
    let f = (x, a) => (x == a)
)
```

A block opened with `{` was never closed with `}`. Instead, an unrelated `)` was found.

✅ **Fix:**

```compose
{
    let f = (x, a) => (x == a)
}
```

---

#### Example 3: Reaching the end of the file without closing

```compose error
{ unclosed_to_eof
```

The block was never closed, and the file ended before the parser could find the matching `}`.

✅ **Fix:**

```compose
{ unclosed_to_eof }
```

---

### Summary

Unclosed delimiters are a syntax error. To fix them:

* Make sure every `(` has a matching `)`, every `{` has a matching `}`, and every `[` has a matching `]`.
* Watch for mismatched pairs, such as opening with `{` and closing with `)`.
* Check for missing closings especially near the end of files or after nested code.

💡 Tip: If you see many unclosed delimiter errors, start by fixing the **first** one. Later ones are often caused by it. 