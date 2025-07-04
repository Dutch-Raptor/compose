use crate::repr::Repr;
use crate::{FromValue, Iter, Trace};
use compose_library::diag::{StrResult, bail};
use compose_library::{IntoValue, IterValue, RangeIter, UntypedRef, Value, Vm};
use compose_macros::{func, scope, ty};
use ecow::{EcoString, eco_format};
use std::sync::Arc;

#[ty(scope, cast, name = "range")]
#[derive(Clone, Debug, PartialEq)]
pub struct RangeValue(Arc<Range>);

#[derive(Clone, Debug, PartialEq)]
pub enum Range {
    Int(RangeImpl<i64>),
    Char(RangeImpl<char>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct RangeImpl<T>
where
    T: PartialEq + PartialOrd,
{
    pub start: Option<T>,
    pub end: Option<T>,
    pub include_end: bool,
}

impl Trace for RangeValue {
    fn visit_refs(&self, _f: &mut dyn FnMut(UntypedRef)) {}
}

impl Range {
    pub fn try_new(start: Option<Value>, end: Option<Value>, include_end: bool) -> StrResult<Self> {
        fn cast_opt<T: FromValue>(v: &Option<Value>) -> Option<Option<T>> {
            let Some(v) = v else {
                return Some(None);
            };

            match v.clone().cast::<T>() {
                Ok(v) => Some(Some(v)),
                Err(_) => None,
            }
        }

        fn cast_opt_pair<T: FromValue>(
            start: &Option<Value>,
            end: &Option<Value>,
        ) -> Option<(Option<T>, Option<T>)> {
            let start = cast_opt(start)?;
            let end = cast_opt(end)?;
            Some((start, end))
        }
        if let Some((start, end)) = cast_opt_pair::<i64>(&start, &end) {
            return Ok(Range::Int(RangeImpl::new(start, end, include_end)));
        }

        if let Some((start, end)) = cast_opt_pair::<char>(&start, &end) {
            return Ok(Range::Char(RangeImpl::new(start, end, include_end)));
        }

        bail!("Cannot create range from types other than int")
    }
}

impl Repr for RangeValue {
    fn repr(&self, _vm: &dyn Vm) -> EcoString {
        let (start, end, include_end) = match &self.0.as_ref() {
            Range::Int(r) => (
                r.start.map(|v| eco_format!("{v}")),
                r.end.map(|e| eco_format!("{e}")),
                r.include_end,
            ),
            Range::Char(r) => (
                r.start.map(|v| eco_format!("{v}")),
                r.end.map(|e| eco_format!("{e}")),
                r.include_end,
            ),
        };
        let op = if include_end { "..=" } else { ".." };
        let start = start.unwrap_or_else(|| eco_format!(""));
        let end = end.unwrap_or_else(|| eco_format!(""));

        eco_format!("{start}{op}{end}")
    }
}

impl<T> RangeImpl<T>
where
    T: PartialEq + PartialOrd,
{
    pub fn new(start: Option<T>, end: Option<T>, include_end: bool) -> Self {
        Self {
            start,
            end,
            include_end,
        }
    }
}

#[scope]
impl RangeValue {
    #[func]
    pub fn new(start: Option<Value>, end: Option<Value>, inclusive_end: bool) -> StrResult<Self> {
        Range::try_new(start, end, inclusive_end)
            .map(Arc::new)
            .map(RangeValue)
    }

    #[func]
    pub fn start(&self) -> Option<Value> {
        match &self.0.as_ref() {
            Range::Int(r) => r.start.map(|v| v.into_value()),
            Range::Char(r) => r.start.map(|v| v.into_value()),
        }
    }
    #[func]
    pub fn end(&self) -> Option<Value> {
        match &self.0.as_ref() {
            Range::Int(r) => r.end.map(|v| v.into_value()),
            Range::Char(r) => r.end.map(|v| v.into_value()),
        }
    }

    #[func]
    pub fn inclusive_end(&self) -> bool {
        match &self.0.as_ref() {
            Range::Int(r) => r.include_end,
            Range::Char(r) => r.include_end,
        }
    }

    #[func]
    pub fn contains(&self, value: Value) -> StrResult<bool> {
        match &self.0.as_ref() {
            Range::Int(r) => Ok(in_bounds(value.cast()?, r.start, r.end, r.include_end)),
            Range::Char(r) => Ok(in_bounds(value.cast()?, r.start, r.end, r.include_end)),
        }
    }

    #[func]
    pub fn iter(self, vm: &mut dyn Vm) -> StrResult<IterValue> {
        Ok(IterValue::new(
            Iter::Range(RangeIter::new(self.inner())?),
            vm,
        ))
    }
}

impl RangeValue {
    pub fn inner(&self) -> &Range {
        &self.0
    }
}

fn in_bounds<T: PartialOrd>(v: T, start: Option<T>, end: Option<T>, include_end: bool) -> bool {
    let lower_ok = start.map_or(true, |s| v >= s);
    let upper_ok = end.map_or(true, |e| if include_end { v <= e } else { v < e });
    lower_ok && upper_ok
}
