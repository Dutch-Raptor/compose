use crate::diag::SourceResult;
use compose_library::diag::{bail, StrResult};
use ecow::eco_format;
use std::sync::{Arc, Mutex};
use compose_library::foundations::cast::IntoValue;
use compose_library::foundations::iterator::ValueIterator;
use compose_library::foundations::types::{Range, Str};
use compose_library::{Value, Vm};

#[derive(Debug, Clone, PartialEq)]
enum RangeIterType {
    Int(i64),
    Char(u32),
}

impl RangeIterType {
    fn nth(&mut self, n: usize) -> Option<RangeIterType> {
        let cur_offset = n;
        let next_offset = cur_offset + 1;
        match self {
            RangeIterType::Int(i) => {
                let return_value = *i + cur_offset as i64;
                *i += next_offset as i64;
                Some(RangeIterType::Int(return_value))
            }
            RangeIterType::Char(c) => {
                if char::from_u32(*c + cur_offset as u32).is_none() {
                    return None;
                };
                *c += next_offset as u32;
                Some(RangeIterType::Char(*c))
            }
        }
    }
}

impl IntoValue for RangeIterType {
    fn into_value(self) -> Value {
        match self {
            RangeIterType::Int(c) => Value::Int(c),
            RangeIterType::Char(c) => Value::Str(Str::from(eco_format!(
                "{}",
                char::from_u32(c).expect("Invalid char code. This is a bug.")
            ))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RangeIter {
    current: Arc<Mutex<RangeIterType>>,
    max_inclusive: bool,
    max: Option<RangeIterType>,
}

impl PartialEq for RangeIter {
    fn eq(&self, other: &Self) -> bool {
        if self.max_inclusive != other.max_inclusive {
            return false;
        }
        if self.max != other.max {
            return false;
        }
        
        let cur = self.current.lock().unwrap();
        let other_cur = other.current.lock().unwrap();
        
        if *cur != *other_cur {
            return false;
        }
        
        true
    }
}

impl RangeIter {
    pub fn new(range: &Range) -> StrResult<Self> {
        let (cur, max, inclusive) = match range {
            Range::Int(r) => {
                (r.start.map(RangeIterType::Int), r.end.map(RangeIterType::Int), r.include_end)
            }
            Range::Char(r) => {
                (r.start.map(|char| RangeIterType::Char(char as u32)), r.end.map(|char| RangeIterType::Char(char as u32)), r.include_end)
            }
        };
        
        let Some(cur) = cur else {
            bail!("Range iterator must have a start value.");
        };
        
        Ok(Self {
            current: Arc::new(Mutex::new(cur)),
            max_inclusive: inclusive,
            max,
        })
    }
}

impl ValueIterator for RangeIter {
    fn next(&self, vm: &mut dyn Vm) -> SourceResult<Option<Value>> {
        self.nth(vm, 0)
    }

    fn nth(&self, _: &mut dyn Vm<'_>, n: usize) -> SourceResult<Option<Value>> {
        let mut current = self.current.lock().unwrap();
        let Some(next) = current.nth(n) else {
            return Ok(None);
        };

        let Some(max) = &self.max else {
            return Ok(Some(next.into_value()));
        };

        let max_ok = match (&next, max) {
            (RangeIterType::Char(c), RangeIterType::Char(m)) => {
                if self.max_inclusive {
                    c <= m
                } else {
                    c < m
                }
            }
            (RangeIterType::Int(c), RangeIterType::Int(m)) => {
                if self.max_inclusive {
                    c <= m
                } else {
                    c < m
                }
            }
            _ => unreachable!("Invalid range iterator state. This is a bug."),
        };

        if !max_ok {
            return Ok(None);
        }

        Ok(Some(next.into_value()))
    }
}
