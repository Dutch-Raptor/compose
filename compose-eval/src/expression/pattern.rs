use crate::evaluated::Evaluated;
use crate::{Eval, Machine};
use compose_error_codes::{E0007_MISSING_EQUALS_AFTER_LET_BINDING, E0301_ARRAY_DESTRUCTURING_WRONG_NUMBER_OF_ELEMENTS, E0302_MAP_DESTRUCTURING_UNCOVERED_KEYS, E0303_MAP_DESTRUCTURING_MISSING_KEY_IN_VALUE, E0304_TYPE_PATTERNS_NOT_ALLOWED_IN_LET_BINDINGS};
use compose_library::diag::{At, SourceDiagnostic, SourceResult, Spanned};
use compose_library::{bail, error, Value, Vm};
use compose_syntax::ast::{AstNode, DestructuringItem, Expr, Ident, LiteralPattern, Pattern};
use compose_syntax::{Span, ast};
use compose_utils::trace_fn;
use ecow::{EcoString, EcoVec, eco_format};
use std::cmp::PartialEq;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use tap::Tap;
use compose_library::foundations::cast::IntoValue;
use compose_library::foundations::ops::Comparison;
use compose_library::foundations::scope::{Binding, BindingKind, Visibility};
use compose_library::foundations::types::{ArrayValue, MapValue, Type};
use compose_library::foundations::value::DebugRepr;

impl Eval for LiteralPattern<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<crate::Evaluated> {
        self.to_untyped()
            .cast::<Expr>()
            .expect("any literal is a valid expression")
            .eval(vm)
    }
}

impl Eval for ast::IsExpression<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        trace_fn!("eval_is_expression");
        let vm = &mut vm.in_flow_scope_guard();
        let expr = self.expr();
        let value = expr.eval(vm)?.value;

        let pat = self.pattern();

        let matched = match destructure_into_flow(vm, value, pat)? {
            PatternMatchResult::NotMatched(_) => false,
            PatternMatchResult::Matched => true,
        };

        Ok(Evaluated::immutable(matched.into_value()))
    }
}

pub fn destructure_into_flow(
    vm: &mut Machine,
    value: Value,
    pat: Pattern,
) -> SourceResult<PatternMatchResult> {
    destructure_impl(
        vm,
        pat,
        value,
        PatternContext::FlowBinding,
        MatchPath::new(),
        &mut |vm, expr, value| {
            match expr {
                Expr::Ident(ident) => {
                    let name = ident.get();
                    let spanned = value
                        .named(Spanned::new(name.clone(), ident.span()))
                        // Now that the names have been added, make sure any deferred errors are resolved
                        .resolved()?;

                    vm.scopes_mut()
                        .top_flow_mut()
                        .expect("running in a flow")
                        .try_bind(name.clone(), Binding::new(spanned, expr.span()))
                        .at(expr.span())?;

                    Ok(())
                }
                _ => bail!(expr.span(), "cannot destructure pattern",),
            }
        },
    )
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum MatchPathSegment {
    ArrayIndex(usize),
    MapKey(EcoString),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct MatchPath {
    segments: EcoVec<MatchPathSegment>,
}

impl MatchPath {
    pub fn new() -> Self {
        Self {
            segments: EcoVec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn with_segment(mut self, segment: MatchPathSegment) -> Self {
        self.segments.push(segment);
        self
    }
}

impl Default for MatchPath {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for MatchPathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MatchPathSegment::ArrayIndex(index) => write!(f, "[{}]", index),
            MatchPathSegment::MapKey(key) => write!(f, ".{}", key),
        }
    }
}

impl Display for MatchPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.segments.iter().try_for_each(|s| write!(f, "{}", s))
    }
}

pub enum PatternMatchResult {
    Matched,
    NotMatched(SourceDiagnostic),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PatternContext {
    LetBinding,
    ForLoopBinding,
    FlowBinding,
    Parameter,
}

pub fn destructure_pattern(
    vm: &mut Machine,
    pattern: Pattern,
    value: Value,
    ctx: PatternContext,
    binding_kind: BindingKind,
    visibility: Visibility,
) -> SourceResult<PatternMatchResult> {
    trace_fn!("eval_destructure_pattern");
    destructure_impl(
        vm,
        pattern,
        value,
        ctx,
        MatchPath::new(),
        &mut |vm, expr, value| match expr {
            Expr::Ident(ident) => {
                let name = ident.get().clone();
                let spanned = value
                    .named(Spanned::new(name, ident.span()))
                    // Now that the names have been added, make sure any deferred errors are resolved
                    .resolved()?;

                vm.define(ident, spanned, binding_kind, visibility)?;

                Ok(())
            }
            _ => bail!(expr.span(), "cannot destructure pattern",),
        },
    )
}

pub fn destructure_impl(
    vm: &mut Machine,
    pattern: Pattern,
    value: Value,
    ctx: PatternContext,
    match_path: MatchPath,
    bind: &mut impl Fn(&mut Machine, Expr, Value) -> SourceResult<()>,
) -> SourceResult<PatternMatchResult> {
    trace_fn!("eval_destructure_impl");
    match pattern {
        Pattern::Single(expr) => {
            if let Some(ident) = expr.cast::<Ident>()
                && let Some(expected_ty) = get_type(vm, ident)
            {
                if &value.ty() != expected_ty {
                    return Ok(PatternMatchResult::NotMatched(
                        error!(ident.span(), "Type pattern does not match";
                    note: "expected `{}`, found `{}`", expected_ty, value.ty()),
                    ));
                }
            }

            bind(vm, expr, value)?;
            Ok(PatternMatchResult::Matched)
        }
        Pattern::TypedPattern(typed_pattern) if ctx == PatternContext::LetBinding => {
            // Types in Compose are resolved at runtime, not during parsing.
            // This means `let x 1` is initially parsed as a typed pattern (`x 1`).
            // We resolve the ambiguity here:
            //   - if `x` resolves to a type → disallowed typed pattern
            //   - otherwise → assume a missing `=` and emit E0007
            let ty = typed_pattern.ty();
            let is_type = get_type(vm, ty).is_some();
            if is_type {
                let pat_text = typed_pattern.pattern().to_text();
                let ty_text = ty.get();
                bail!(
                    typed_pattern.span(),
                    "type patterns are not allowed in `let` bindings";
                    note: "`let {ty_text} {pat_text} = ...` is not a type annotation in Compose";
                    hint: "use a `match` or `is` expression to check the type of a value";
                    code: &E0304_TYPE_PATTERNS_NOT_ALLOWED_IN_LET_BINDINGS;
                );
            } else {
                let binding_text = ty.get();
                bail!(ty.span().after(), "expected `=` after binding name";
                    label_message: "expected `=` here";
                    hint: "if you meant to initialise the binding, add `=`: `let {binding_text} = ...;`";
                    hint: "or, if you meant to leave it uninitialised, add a semicolon `let {binding_text};`";
                    code: &E0007_MISSING_EQUALS_AFTER_LET_BINDING)
            }
        }
        Pattern::TypedPattern(typed_pattern) => {
            let ty_ident = typed_pattern.ty();

            let expected_ty = get_type(vm, ty_ident);
            let Some(expected_ty) = expected_ty else {
                bail!(typed_pattern.ty().span(), "Type in typed pattern does not exist";)
            };

            if &value.ty() != expected_ty {
                return Ok(PatternMatchResult::NotMatched(
                    error!(ty_ident.span(), "Type pattern does not match";
                    note: "expected `{}`, found `{}`", expected_ty, value.ty()),
                ));
            }

            destructure_impl(vm, typed_pattern.pattern(), value, ctx, match_path, bind)
        }
        Pattern::PlaceHolder(_) => Ok(PatternMatchResult::Matched), // A placeholder means we discard the value, no need to bind
        Pattern::Destructuring(destruct) => match value {
            Value::Array(value) => destructure_array(vm, destruct, value, ctx, match_path, bind),
            Value::Map(value) => destructure_map(vm, destruct, value, ctx, match_path, bind),
            _ => bail!(pattern.span(), "cannot destructure {}", value.ty()),
        },
        Pattern::LiteralPattern(lit) if ctx == PatternContext::LetBinding => bail!(
            lit.span(),
            "literal patterns are not allowed in `let` bindings";
            note: "a `let` binding must introduce at least one variable"
        ),
        Pattern::LiteralPattern(lit) if ctx == PatternContext::ForLoopBinding => bail!(
            lit.span(),
            "literal patterns are not allowed in `let` bindings";
            note: "a `for` llop binding must introduce at least one variable"
        ),
        Pattern::LiteralPattern(lit) => {
            let literal_value = lit.eval(vm)?.value;

            if literal_value.not_equals(&value, &vm.heap).at(lit.span())? {
                return Ok(PatternMatchResult::NotMatched(error!(
                    lit.span(),
                    "literal pattern did not match";
                    label_message: "expected `{}`, got `{}`", literal_value.debug_repr(vm), value.debug_repr(vm);
                    note: "while matching the value at path {}", match_path;
                )));
            }

            Ok(PatternMatchResult::Matched)
        }
    }
}

fn get_type<'a>(vm: &mut Machine<'a>, ty_ident: Ident) -> Option<&'a Type> {
    match vm
        .engine
        .world
        .library()
        .global
        .scope()
        .get(ty_ident.get())
        .map(|b| b.read())?
    {
        Value::Type(t) => Some(t),
        _ => None,
    }
}

fn destructure_array(
    vm: &mut Machine,
    destruct: ast::Destructuring,
    value: ArrayValue,
    ctx: PatternContext,
    match_path: MatchPath,
    bind: &mut impl Fn(&mut Machine, Expr, Value) -> SourceResult<()>,
) -> SourceResult<PatternMatchResult> {
    let arr = value.heap_ref().get_unwrap(&vm.heap).clone();

    let len = arr.len();
    let mut index = 0;

    for p in destruct.items() {
        match p {
            DestructuringItem::Pattern(pat) => {
                let Some(v) = arr.get(index) else {
                    return Ok(PatternMatchResult::NotMatched(wrong_number_of_elements(
                        destruct,
                        len,
                        &match_path,
                    )));
                };

                let matched = destructure_impl(
                    vm,
                    pat,
                    v.clone(),
                    ctx,
                    match_path
                        .clone()
                        .with_segment(MatchPathSegment::ArrayIndex(index)),
                    bind,
                )?;

                if let PatternMatchResult::NotMatched(err) = matched {
                    return Ok(PatternMatchResult::NotMatched(err));
                }
                index += 1;
            }
            DestructuringItem::Named(named) => {
                return Ok(PatternMatchResult::NotMatched(error!(
                    named.span(),
                    "cannot destructure a named pattern from an array"
                )));
            }
            DestructuringItem::Spread(spread) => {
                // The number of elements that have not been bound by a destructuring item and will be bound by the spread
                let sink_size = (1 + len).checked_sub(destruct.items().count());

                // The items that will be bound by the spread
                let sunk_items = sink_size.and_then(|n| arr.get(index..index + n));

                let (Some(sink_size), Some(sunk_items)) = (sink_size, sunk_items) else {
                    return Ok(PatternMatchResult::NotMatched(wrong_number_of_elements(
                        destruct,
                        len,
                        &match_path,
                    )));
                };

                if let Some(expr) = spread.sink_expr() {
                    let sunk_arr =
                        ArrayValue::from(vm.heap_mut(), sunk_items.to_vec()).into_value();
                    bind(vm, expr, sunk_arr)?;
                }
                index += sink_size;
            }
        }
    }

    // require all items to be bound
    if index != len {
        return Ok(PatternMatchResult::NotMatched(wrong_number_of_elements(
            destruct,
            len,
            &match_path,
        )));
    }

    Ok(PatternMatchResult::Matched)
}

#[allow(unused)]
fn destructure_map(
    vm: &mut Machine,
    destruct: ast::Destructuring,
    value: MapValue,
    ctx: PatternContext,
    match_path: MatchPath,
    bind: &mut impl Fn(&mut Machine, Expr, Value) -> SourceResult<()>,
) -> SourceResult<PatternMatchResult> {
    let map = value.heap_ref().get_unwrap(&vm.heap).clone();

    let mut used: HashSet<&str> = HashSet::new();
    let mut spread = None;

    for p in destruct.items() {
        match p {
            DestructuringItem::Named(named) => {
                let name = named.name().get();
                used.insert(name);

                let Some(v) = map.get(name) else {
                    return Ok(PatternMatchResult::NotMatched(missing_key_err(
                        name,
                        named.span(),
                        &match_path,
                    )));
                };

                let matched = destructure_impl(
                    vm,
                    named.pattern(),
                    v.clone(),
                    ctx,
                    match_path
                        .clone()
                        .with_segment(MatchPathSegment::MapKey(name.clone())),
                    bind,
                )?;
                if let PatternMatchResult::NotMatched(err) = matched {
                    return Ok(PatternMatchResult::NotMatched(err));
                }
            }
            DestructuringItem::Pattern(Pattern::Single(Expr::Ident(ident))) => {
                used.insert(ident.get());
                let Some(v) = map.get(ident.get()) else {
                    return Ok(PatternMatchResult::NotMatched(missing_key_err(
                        ident.get(),
                        ident.span(),
                        &match_path,
                    )));
                };

                bind(vm, Expr::Ident(ident.clone()), v.clone())?;
            }
            DestructuringItem::Spread(s) => {
                spread = Some(s);
            }
            DestructuringItem::Pattern(_) => {
                let mut err = error!(
                    destruct.span(),
                    "cannot destructure an unnamed pattern from a map"
                );

                if !match_path.is_empty() {
                    err.note(eco_format!(
                        "while matching the pattern at path {}",
                        match_path
                    ))
                }

                return Ok(PatternMatchResult::NotMatched(err));
            }
        }
    }

    if let Some(spread) = spread
        && spread.sink_expr().is_some()
    {
        let map = HashMap::from_iter(
            map.iter()
                .filter(|(k, _)| !used.contains(k.as_str()))
                .map(|(k, v)| (k.clone(), v.clone())),
        );
        let value = MapValue::from(vm.heap_mut(), map).into_value();
        bind(vm, spread.sink_expr().unwrap(), value)?;
    }

    if spread.is_none() && used.len() != map.len() {
        let key_hint = {
            let missing_keys = map
                .keys()
                .filter(|k| !used.contains(k.as_str()))
                .map(|k| k.as_str())
                .collect::<Vec<_>>()
                .tap_mut(|v| v.sort_unstable());
            format_keyset(&missing_keys)
        };
        let mut err = error!(
            destruct.span(),
            "map destructuring does not cover all keys";
            label_message: "unmatched keys: {key_hint}";
            hint: "add `..` to ignore the remaining keys";
            hint: "or use `..name` to bind them into a map";
            code: &E0302_MAP_DESTRUCTURING_UNCOVERED_KEYS,
        );

        if !match_path.is_empty() {
            err.note(eco_format!(
                "while matching the pattern at path {}",
                match_path
            ))
        }

        return Ok(PatternMatchResult::NotMatched(err));
    }

    Ok(PatternMatchResult::Matched)
}

fn missing_key_err(name: &str, span: Span, match_path: &MatchPath) -> SourceDiagnostic {
    let mut err = error!(
        span, "missing key in map pattern";
        label_message: "key `{}` is not present in the map", name;
        code: &E0303_MAP_DESTRUCTURING_MISSING_KEY_IN_VALUE;
    );

    if !match_path.is_empty() {
        err.note(eco_format!(
            "while matching the pattern at path {}",
            match_path
        ))
    }

    err
}

/// Returns a string describing a keyset. lists the first 3 keys, and lists the count of remaining keys.
fn format_keyset(missing_keys: &[&str]) -> EcoString {
    const LISTED_KEYS_NUM: usize = 3;
    let named_keys = missing_keys
        .iter()
        .take(LISTED_KEYS_NUM)
        .copied()
        .collect::<Vec<_>>()
        .join(", ");
    let rest_len = missing_keys.len().saturating_sub(LISTED_KEYS_NUM);

    let more = if rest_len > 0 {
        &eco_format!(", and {rest_len} more")
    } else {
        ""
    };
    eco_format!("{}{}", named_keys, more)
}

/// Returns a diagnostic indicating that the number of elements in the array does not match the number of elements in the destructuring pattern.
#[cold]
fn wrong_number_of_elements(
    destruct: ast::Destructuring,
    len: usize,
    match_path: &MatchPath,
) -> SourceDiagnostic {
    let mut count = 0;
    let mut spread = false;

    for p in destruct.items() {
        match p {
            DestructuringItem::Pattern(_) => count += 1,
            DestructuringItem::Spread(_) => spread = true,
            DestructuringItem::Named(_) => {}
        }
    }

    let quantifier = if len > count {
        "too many"
    } else {
        "not enough"
    };
    let expected = match (spread, count) {
        (true, 1) => "at least 1 element".into(),
        (true, c) => eco_format!("at least {c} elements"),
        (false, 0) => "an empty array".into(),
        (false, 1) => "a single element".into(),
        (false, c) => eco_format!("{c} elements",),
    };

    let mut err = error!(
        destruct.span(), "{quantifier} elements to destructure";
        hint: "the provided array has a length of {len}, \
               but the pattern expects {expected}";
        code: &E0301_ARRAY_DESTRUCTURING_WRONG_NUMBER_OF_ELEMENTS;
    );

    if len > count {
        err.hint("use `..` to ignore the remaining elements, or `..name` to bind them");
    }

    if !match_path.is_empty() {
        err.note(eco_format!(
            "while matching the pattern at path {}",
            match_path
        ))
    }

    err
}

#[cfg(test)]
mod tests {
    use crate::test::{assert_eval, eval_code};
    use compose_error_codes::{
        E0301_ARRAY_DESTRUCTURING_WRONG_NUMBER_OF_ELEMENTS, E0302_MAP_DESTRUCTURING_UNCOVERED_KEYS,
        E0303_MAP_DESTRUCTURING_MISSING_KEY_IN_VALUE,
    };

    #[test]
    fn simple_map_destructuring() {
        assert_eval(
            r#"
            let { a, b } = { a: 1, b: 2 };
            assert::eq(a, 1);
            assert::eq(b, 2);
        "#,
        );
    }

    #[test]
    fn simple_array_destructuring() {
        assert_eval(
            r#"
            let [a, b] = [1, 2];
            assert::eq(a, 1);
            assert::eq(b, 2);
        "#,
        );
    }

    #[test]
    fn named_map_destructuring() {
        assert_eval(
            r#"
            let { a: x, b: y } = { a: 1, b: 2 };
            assert::eq(x, 1);
            assert::eq(y, 2);
        "#,
        );
    }

    #[test]
    fn nested_named_map_destructuring() {
        assert_eval(
            r#"
            let { a: { x: z } } = { a: { x: 1 } };
            assert::eq(z, 1);
        "#,
        );
    }

    #[test]
    fn spread_map_destructuring() {
        assert_eval(
            r#"
            let { a, ..rest } = { a: 1, b: 2 };
            assert::eq(a, 1);
            assert::eq(rest.get("b"), 2);
        "#,
        );
    }

    #[test]
    fn spread_array_destructuring() {
        assert_eval(
            r#"
            let [a, ..rest] = [1, 2];
            assert::eq(a, 1);
            assert::eq(rest, [2]);
        "#,
        );
    }

    #[test]
    fn literal_pattern() {
        assert_eval(
            r#"
            assert([1, 2] is [1, _]);
        "#,
        );
    }

    #[test]
    fn map_error_with_unmapped_keys() {
        eval_code(
            r#"
            let { a } = { a: 1, c: 2 };
        "#,
        )
        .assert_errors(&[&E0302_MAP_DESTRUCTURING_UNCOVERED_KEYS]);
    }

    #[test]
    fn map_error_with_nonexistent_key() {
        // TODO: Add error code for nonexistent key
        eval_code(
            r#"
            let { a } = { b: 1 };
        "#,
        )
        .assert_errors(&[&E0303_MAP_DESTRUCTURING_MISSING_KEY_IN_VALUE]);
    }

    #[test]
    fn array_error_with_uncovered_elements() {
        eval_code(
            r#"
            let [a] = [1, 2];
        "#,
        )
        .assert_errors(&[&E0301_ARRAY_DESTRUCTURING_WRONG_NUMBER_OF_ELEMENTS]);
    }

    #[test]
    fn array_error_with_overcovered_elements() {
        eval_code(
            r#"
            let [a, b] = [1];
        "#,
        )
        .assert_errors(&[&E0301_ARRAY_DESTRUCTURING_WRONG_NUMBER_OF_ELEMENTS]);
    }
}
