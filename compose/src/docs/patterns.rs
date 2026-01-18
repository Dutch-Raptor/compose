use compose_doc_macros::compose_doc;

compose_doc! {
/// # Pattern Matching in Compose
///
/// Compose provides powerful tools for inspecting and decomposing values using
/// destructuring, `is` expressions, and `match` expressions. These let you
/// ergonomically check a value's structure, bind parts of it to variables, and
/// conditionally execute code based on patterns.
///
/// ## Destructuring with `let`
///
/// You can destructure arrays, tuples, and other composite values directly in
/// `let` bindings:
///
/// ```compose
/// let array = [1, "a string", 3];
///
/// // Destructure first element and the rest of the array
/// let [first, ..rest] = array;
/// assert::eq(first, 1);
/// assert::eq(rest.len(), 2);
/// ```
///
/// Here, `first` is bound to the first element, and `rest` captures the remaining elements.
///
/// ## `is` Expressions
///
/// Use `is` to check the structure or type of a value and optionally bind variables
/// when the check succeeds:
///
/// ```compose
/// # let array = [1, "a string", 3];
/// if array is [_, String s, ..] && s.len() == 8 {
///     assert::eq(s, "a string"); // s is accessible within the if body
/// };
/// ```
///
/// The pattern `[_, String s, ..]` matches any array whose second element is a string,
/// binding it to `s`.
///
/// ## `match` Expressions
///
/// `match` lets you handle multiple patterns and conditions:
///
/// ```compose
/// # let array = [1, "a string", 3];
/// match (array) {
///     [_, _] => "any 2 elements",
///     [Int x, Int y, ..] => "array starting with 2 ints, bound to x and y",
///     [Int x, ..] if x % 2 == 0 => "array starting with an even int, bound to x",
///     [1, "a string", ..] => "array starting with 1 and a string",
///     other => "anything else, bound to variable `other`",
/// };
/// ```
///
/// Notes:
/// - Patterns are checked in order, and the first match is taken.
/// - Variables bound in a pattern are available in the corresponding branch body.
/// - Guards (`if` clauses) can add additional conditions to a pattern.
///
/// ## Destructuring in Function Parameters
///
/// Patterns can also be used in function parameters for concise extraction:
///
/// ```compose
/// # let array = [1, "a string", 3];
/// let first = { [first, ..] => first };
/// assert::eq(first(array), 1);
/// ```
///
/// This defines a function that takes an array, destructures it, and returns the
/// first element.
///
/// ## Summary
///
/// - Use `let` for destructuring values in local bindings.
/// - Use `is` for conditional type/structure checks with optional bindings.
/// - Use `match` for multi-case branching with pattern matching.
/// - Destructuring can also appear in function parameters for concise, readable code.
    pub mod Patterns {}
}
