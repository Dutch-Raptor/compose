use crate::repr::Repr;
use crate::{FromValue, Trace};
use compose_library::diag::{StrResult, bail};
use compose_library::{UntypedRef, Value, Vm};
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
    start: Option<T>,
    end: Option<T>,
    include_end: bool,
}

impl Trace for RangeValue {
    fn visit_refs(&self, _f: &mut dyn FnMut(UntypedRef)) {}
}

impl Range {
    pub fn try_new(start: Option<Value>, end: Option<Value>, include_end: bool) -> StrResult<Self> {
        fn cast_opt<T: FromValue>(v: Option<Value>) -> Option<Option<T>> {
            v.map(|v| v.cast::<T>().ok())
        }
        if let Some(start) = cast_opt::<i64>(start)
            && let Some(end) = cast_opt::<i64>(end)
        {
            return Ok(Range::Int(RangeImpl::new(start, end, include_end)));
        }

        bail!("Cannot create range from types other than int and char")
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
}
