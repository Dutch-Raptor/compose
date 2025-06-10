use crate::diag::{At, SourceResult, Spanned};
use crate::foundations::boxed::Boxed;
use crate::foundations::iterator::IterValue;
use crate::{CastInfo, Str, UnitValue};
use crate::{FromValue, Func};
use crate::{IntoValue, ValueRef};
use crate::{NativeScope, Reflect};
use crate::{Sink, Type};
use compose_library::diag::{bail, error, StrResult};
use compose_library::repr::Repr;
use compose_macros::func;
use compose_macros::scope;
use compose_syntax::Span;
use dumpster::{Trace, Visitor};
use ecow::{eco_format, EcoString};
use std::collections::HashSet;
use std::fmt;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit(UnitValue),
    Str(Str),
    Func(Func),
    Type(Type),
    Iterator(IterValue),
    Box(Boxed),
}

#[scope]
impl Value {
    #[func(name = "repr")]
    pub fn repr_(self) -> EcoString {
        self.repr()
    }
}

impl Value {
    pub fn is_box(&self) -> bool {
        matches!(self, Value::Box(_))
    }
}

fn eq_internal(l_ref: ValueRef, r_ref: ValueRef, visited: &mut HashSet<(usize, usize)>) -> bool {
    use Value::*;

    let l = l_ref.deref();
    let r = r_ref.deref();

    // If both are boxes, compare the inner pointers
    if let (Box(boxed_l), Box(boxed_r)) = (l, r) {
        let ptr_l = boxed_l.as_ptr() as usize;
        let ptr_r = boxed_r.as_ptr() as usize;

        if !visited.insert((ptr_l, ptr_r)) {
            return true; // we've seen this pair already
        }

        let Ok(inner_l) = boxed_l.as_ref() else {
            return false;
        };
        let Ok(inner_r) = boxed_r.as_ref() else {
            return false;
        };
        return eq_internal(inner_l, inner_r, visited);
    }

    // If one is a box and the other isn't, unwrap and compare
    if let Box(boxed_l) = l {
        let Ok(inner_l) = boxed_l.as_ref() else {
            return false;
        };
        return eq_internal(inner_l, r_ref, visited);
    }

    if let Box(boxed_r) = r {
        let Ok(inner_r) = boxed_r.as_ref() else {
            return false;
        };
        return eq_internal(l_ref, inner_r, visited);
    }

    macro_rules! cmp {
            ($($variant:ident),* $(,)?) => {
                match (l.deref(), r.deref()) {
                    // If the unboxed value is still a boxed value, unbox it
                    (Value::Box(_), _) |
                    (_, Value::Box(_)) => unreachable!("Boxes should have been unboxed before comparing"),

                    $(
                        (Value::$variant(l), Value::$variant(r)) => l == r,
                    )*

                    // Write out $variant, _ to keep the match exhaustive so that
                    // new variants will cause a compile error here.
                    $((Value::$variant(_), _) => false),*,
                }
            }
        }

    cmp![Int, Bool, Unit, Str, Func, Type, Iterator]
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        // A boxed value can be the same as another unboxed value
        // so we need to unbox both values before comparing them
        let Ok(l) = self.as_ref() else { return false };
        let Ok(r) = other.as_ref() else { return false };

        let mut visited = HashSet::new();
        eq_internal(l, r, &mut visited)
    }
}

unsafe impl Trace for Value {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            Value::Int(_) => {}
            Value::Bool(_) => {}
            Value::Unit(_) => {}
            Value::Str(_) => {}
            Value::Func(_) => {}
            Value::Type(_) => {}
            Value::Iterator(i) => i.accept(visitor)?,
            Value::Box(b) => b.accept(visitor)?,
        }

        Ok(())
    }
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Value::Int(_) => Type::of::<i64>(),
            Value::Bool(_) => Type::of::<bool>(),
            Value::Unit(_) => Type::of::<UnitValue>(),
            Value::Str(_) => Type::of::<Str>(),
            Value::Func(_) => Type::of::<Func>(),
            Value::Type(_) => Type::of::<Type>(),
            Value::Iterator(_) => Type::of::<IterValue>(),
            Value::Box(_) => Type::of::<Boxed>(),
        }
    }

    pub fn unit() -> Value {
        Value::Unit(UnitValue)
    }

    pub fn cast<T: FromValue>(self) -> StrResult<T> {
        T::from_value(self)
    }

    pub fn spanned(self, span: Span) -> Self {
        match self {
            Value::Func(v) => Value::Func(v.spanned(span)),
            _ => self,
        }
    }

    pub fn named(self, name: Spanned<EcoString>) -> Self {
        match self {
            Value::Func(v) => Value::Func(v.named(name)),
            _ => self,
        }
    }

    pub fn resolved(self) -> SourceResult<Self> {
        match self {
            Value::Func(f) => {
                f.resolve()?;
                Ok(Value::Func(f))
            }
            _ => Ok(self),
        }
    }

    /// Access a field on this value (with dot syntax) `a.b`
    pub fn field(&self, field: &str, access_span: Span, sink: &mut Sink) -> StrResult<Value> {
        dbg!(Value::scope());
        let field_value = match self {
            Self::Func(func) => func.field(field, access_span, sink).cloned(),
            Self::Type(ty) => ty.field(field, access_span, sink).cloned(),
            _ => Err(error!(
                "no field or method named `{}` on `{}`",
                field,
                self.ty()
            )),
        };

        if let Ok(field_value) = field_value {
            return Ok(field_value);
        }

        // Get the field from the type
        let type_field_value = self.ty().field(field, access_span, sink).cloned();
        if let Ok(type_field_value) = type_field_value {
            return Ok(type_field_value);
        }

        // Try and get the type from Value itself
        let self_field_value = Value::scope()
            .get(field)
            .map(|b| b.read_checked(access_span, sink))
            .cloned();

        match self_field_value {
            None => bail!("no field or method named `{}` on `{}`", field, self.ty()),
            Some(v) => Ok(v),
        }
    }

    /// Access an associated value of this value by path syntax `a::b`
    pub fn path(&self, path: &str, access_span: Span, sink: &mut Sink) -> SourceResult<Value> {
        match self {
            Self::Type(ty) => ty.path(path, access_span, sink).cloned().at(access_span),
            Self::Func(func) => func.path(path, access_span, sink).cloned().at(access_span),
            _ => bail!(
                access_span,
                "no associated field or method named `{}` on `{}`",
                path,
                self.ty()
            ),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Unit(_) => write!(f, "()"),
            Value::Str(v) => write!(f, "{}", v),
            Value::Func(v) => write!(f, "{}", v),
            Value::Type(v) => write!(f, "{}", v),
            Value::Iterator(v) => write!(f, "{:?}", v),
            Value::Box(v) => write!(f, "{}", v),
        }
    }
}

impl Repr for Value {
    fn repr(&self) -> EcoString {
        match self {
            Value::Int(v) => eco_format!("{v}"),
            Value::Bool(v) => eco_format!("{v}"),
            Value::Unit(_) => eco_format!("()"),
            Value::Str(v) => v.repr(),
            Value::Func(v) => eco_format!("{v}"),
            Value::Type(v) => eco_format!("{v}"),
            Value::Iterator(v) => eco_format!("iterator({v:?})"),
            Value::Box(v) => eco_format!("{v}"),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Unit(UnitValue)
    }
}

/// Implements traits for primitives (Value enum variants).
macro_rules! primitive {
    (
        $ty:ty: $name:literal, $variant:ident
        $(, $other:ident$(($binding:ident))? => $out:expr)*
    ) => {
        impl Reflect for $ty {
            fn input() -> CastInfo {
                CastInfo::Type(Type::of::<Self>())
            }

            fn output() -> CastInfo {
                CastInfo::Type(Type::of::<Self>())
            }

            fn castable(value: &Value) -> bool {
                matches!(value, Value::$variant(_)
                    $(|  primitive!(@$other $(($binding))?))*)
            }
        }

        impl IntoValue for $ty {
            fn into_value(self) -> Value {
                Value::$variant(self)
            }
        }

        impl FromValue for $ty {
            fn from_value(value: Value) -> StrResult<Self> {
                match value {
                    Value::$variant(v) => Ok(v),
                    $(Value::$other$(($binding))? => Ok($out),)*
                    v => Err(<Self as Reflect>::error(&v)),
                }
            }
        }
    };

    (@$other:ident($binding:ident)) => { Value::$other(_) };
    (@$other:ident) => { Value::$other };
}

primitive!(i64: "int", Int);
primitive!(bool: "bool", Bool);
primitive!(Str: "str", Str);
primitive!(Func: "func", Func);
primitive!(Type: "type", Type);
primitive!(UnitValue: "unit", Unit);
primitive!(IterValue: "iterator", Iterator);
primitive!(Boxed: "box", Box);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unbox_partial_eq() {
        let a = Value::Box(Boxed::new(Value::Box(Boxed::new(Value::Int(5)))));
        let b = Value::Int(5);
        assert_eq!(a, b);
    }

    #[test]
    fn test_unbox_partial_eq_cycles() {
        let mut boxed = Value::Box(Boxed::new(Value::Int(5)));
        // now reassign the boxed value to itself
        *boxed.as_mut().unwrap() = boxed.clone();

        assert_eq!(boxed, boxed);
    }
}
