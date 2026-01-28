## E0304: Type patterns are not allowed in `let` bindings

This error occurs when a `let` binding uses a *type pattern*, such as:

```compose error(E0304)
let Int x = 1;
```

In Compose, **`let` bindings do not perform type checks**. A binding like `let x = value;` simply introduces a name and assigns it a value. 
Writing a type before the binding name is **not** treated as a type annotation.

### Why this is not allowed

Although Compose supports *type patterns* (for example `Int x`), those patterns are only meaningful in contexts where **control flow can branch**, such as `match` or `is` expressions. A plain `let` binding has no way to handle the case where the value does *not* match the type, so allowing type patterns there would be misleading.

In other words:

* `let Int x = ...` looks like a static type annotation
* but Compose does not use `let` for type checking
* and failed type matches must be handled explicitly

To avoid unexpected runtime errors, type patterns are not allowed in `let` bindings.

### How to fix this

If you simply want to bind a value:

```compose
let x = 1;
```

If you want to **check the type of a value**, use a `match` expression:

```compose
# let value = 1;
match (value) {
    Int x => println(x),
    _ => panic("expected an Int"),
}
```

Or use an `is` expression:

```compose
# let value = 1;
if (value is Int x) {
    println(x);
}
```

### Summary

* `let` bindings introduce names, they do not check types
* `let Int x = ...` is **not** a type annotation in Compose
* use `match` or `is` when you need to branch on a value’s type