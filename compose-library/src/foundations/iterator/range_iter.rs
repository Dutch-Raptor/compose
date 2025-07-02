use crate::diag::SourceResult;
use crate::Range;
use compose_library::diag::{bail, StrResult};
use compose_library::{IntoValue, Str, Value, ValueIterator, Vm};
use ecow::eco_format;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, PartialEq)]
enum RangeIterType {
    Int(i64),
    Char(u32),
}

impl RangeIterType {
    fn advance(&mut self) -> Option<RangeIterType> {
        match self {
            RangeIterType::Int(i) => {
                let return_value = *i;
                *i += 1;
                Some(RangeIterType::Int(return_value))
            }
            RangeIterType::Char(c) => {
                if char::from_u32(*c).is_none() {
                    return None;
                };
                *c += 1;
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
    pub fn new(range: Range) -> StrResult<Self> {
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

impl Value {
    fn next(&self) -> StrResult<Value> {
        Ok(match self {
            Value::Int(v) => Value::Int(v + 1),
            _ => bail!("Cannot increment value of type {}", self.ty().name()),
        })
    }
}

impl ValueIterator for RangeIter {
    fn next(&self, _: &mut dyn Vm<'_>) -> SourceResult<Option<Value>> {
        let mut current = self.current.lock().unwrap();
        let Some(next) = current.advance() else {
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
