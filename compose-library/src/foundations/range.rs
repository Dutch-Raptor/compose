use std::sync::Arc;
use ecow::{eco_format, EcoString};
use compose_library::{UntypedRef, Value, Vm};
use compose_macros::{scope, ty, func};
use crate::repr::Repr;
use crate::Trace;

#[ty(scope, cast, name = "range",)]
#[derive(Clone, Debug, PartialEq)]
pub struct RangeValue(Arc<Range>);

#[derive(Clone, Debug, PartialEq)]
pub struct Range {
    start: Option<Value>,
    end: Option<Value>,
    include_end: bool,
}

impl Trace for RangeValue {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        if let Some(start) = &self.0.start {
            start.visit_refs(f);
        }
        if let Some(end) = &self.0.end {
            end.visit_refs(f);
        }
    }
}

impl Repr for RangeValue {
    fn repr(&self, vm: &dyn Vm) -> EcoString {
        let op = self.0.include_end.then(|| "..=").unwrap_or_else(|| "..");
        
        let start = self.0.start.as_ref().map(|v| v.repr(vm)).unwrap_or_default();
        let end = self.0.end.as_ref().map(|v| v.repr(vm)).unwrap_or_default();
        
        eco_format!("{start}{op}{end}")
    }
}

impl Range {
    pub fn new(start: Option<Value>, end: Option<Value>, include_end: bool) -> Self {
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
    pub fn new(start: Option<Value>, end: Option<Value>, inclusive_end: bool) -> Self {
        Self(Arc::new(Range::new(start, end, inclusive_end)))
    }
}