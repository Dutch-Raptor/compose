use crate::{Eval, Machine};
use compose_error_codes::E0301_ARRAY_DESTRUCTURING_WRONG_NUMBER_OF_ELEMENTS;
use compose_library::diag::{At, SourceDiagnostic, SourceResult, Spanned};
use compose_library::ops::Comparison;
use compose_library::{
    ArrayValue, BindingKind, DebugRepr, IntoValue, Value, Visibility, Vm, bail, error,
};
use compose_syntax::ast;
use compose_syntax::ast::{AstNode, DestructuringItem, Expr, LiteralPattern, Pattern};
use ecow::{EcoString, EcoVec, eco_format};
use std::cmp::PartialEq;
use std::fmt::Display;

impl Eval for LiteralPattern<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<crate::Evaluated> {
        self.to_untyped()
            .cast::<Expr>()
            .expect("any literal is a valid expression")
            .eval(vm)
    }
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

    pub fn len(&self) -> usize {
        self.segments.len()
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
    Match,
    LetBinding,
    ForLoopBinding,
}

pub fn destructure_pattern(
    vm: &mut Machine,
    pattern: Pattern,
    value: Value,
    ctx: PatternContext,
    binding_kind: BindingKind,
    visibility: Visibility,
) -> SourceResult<PatternMatchResult> {
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

fn destructure_impl(
    vm: &mut Machine,
    pattern: Pattern,
    value: Value,
    ctx: PatternContext,
    match_path: MatchPath,
    bind: &mut impl Fn(&mut Machine, Expr, Value) -> SourceResult<()>,
) -> SourceResult<PatternMatchResult> {
    match pattern {
        Pattern::Single(expr) => {
            bind(vm, expr, value)?;
            Ok(PatternMatchResult::Matched)
        }
        Pattern::PlaceHolder(_) => Ok(PatternMatchResult::Matched), // A placeholder means we discard the value, no need to bind
        Pattern::Destructuring(destruct) => match value {
            Value::Array(value) => destructure_array(vm, destruct, value, ctx, match_path, bind),
            Value::Map(value) => destructure_map(vm, destruct, value, ctx, bind),
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
    value: compose_library::MapValue,
    ctx: PatternContext,
    bind: &mut impl Fn(&mut Machine, Expr, Value) -> SourceResult<()>,
) -> SourceResult<PatternMatchResult> {
    unimplemented!("destructuring maps")
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
