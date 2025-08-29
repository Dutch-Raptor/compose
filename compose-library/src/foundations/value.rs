use crate::diag::{At, SourceResult, Spanned};
use crate::foundations::boxed::Boxed;
use crate::{CastInfo, Str, SyntaxContext, UnitValue};
use crate::{FromValue, Func};
use crate::{IntoValue, Vm};
use crate::{NativeScope, Reflect};
use crate::{Sink, Type};
use compose_library::diag::{StrResult, bail, error};
use compose_library::foundations::range::RangeValue;
use compose_library::repr::Repr;
use compose_library::{Args, ArrayValue, IterValue, MapValue, Module};
use compose_macros::func;
use compose_macros::scope;
use compose_syntax::Span;
use ecow::{EcoString, eco_format};
use std::{fmt, iter};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit(UnitValue),
    Str(Str),
    Func(Func),
    Type(Type),
    Iterator(IterValue),
    Box(Boxed),
    Array(ArrayValue),
    Range(RangeValue),
    Map(MapValue),
    Module(Module),
}

#[scope]
impl Value {
    #[func(name = "to_string")]
    pub fn to_string(self, vm: &dyn Vm) -> EcoString {
        self.repr(vm)
    }

    #[func(name = "clone")]
    pub fn shallow_clone(self, vm: &mut dyn Vm) -> StrResult<Self> {
        Ok(match self {
            Value::Int(v) => Value::Int(v),
            Value::Bool(v) => Value::Bool(v),
            Value::Unit(v) => Value::Unit(v),
            Value::Str(v) => Value::Str(v),
            Value::Func(v) => Value::Func(v),
            Value::Type(v) => Value::Type(v),
            Value::Iterator(v) => Value::Iterator(v.shallow_clone(vm)),
            Value::Box(v) => Value::Box(v.shallow_clone(vm)),
            Value::Array(v) => Value::Array(v.shallow_clone(vm)),
            Value::Range(v) => Value::Range(v),
            Value::Map(v) => Value::Map(v.shallow_clone(vm)),
            Value::Module(v) => Value::Module(v.clone()),
        })
    }

    #[func]
    pub fn tap(self, vm: &mut dyn Vm, side_effect: Func) -> SourceResult<Self> {
        vm.call_func(
            &side_effect,
            Args::new(side_effect.span, iter::once(self.clone())),
        )?;
        Ok(self)
    }

    #[func]
    pub fn pipe(self, vm: &mut dyn Vm, transform: Func) -> SourceResult<Self> {
        vm.call_func(&transform, Args::new(transform.span, iter::once(self)))
    }
}

impl Value {
    pub fn is_box(&self) -> bool {
        matches!(self, Value::Box(_))
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
            Value::Array(_) => Type::of::<ArrayValue>(),
            Value::Range(_) => Type::of::<RangeValue>(),
            Value::Map(_) => Type::of::<MapValue>(),
            Value::Module(_) => Type::of::<Module>(),
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
    pub fn path(
        &self,
        path: &str,
        access_span: Span,
        sink: &mut Sink,
        ctx: &SyntaxContext,
    ) -> SourceResult<Value> {
        match self {
            Self::Type(ty) => ty.path(path, access_span, sink).cloned().at(access_span),
            Self::Func(func) => func.path(path, access_span, sink).cloned().at(access_span),
            Self::Module(module) => module.path(path, access_span, sink, ctx).cloned(),
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
            Value::Array(v) => write!(f, "{:?}", v),
            Value::Range(v) => write!(f, "{:?}", v),
            Value::Map(v) => write!(f, "{:?}", v),
            Value::Module(v) => write!(f, "{}", v.name()),
        }
    }
}

impl Repr for Value {
    fn repr(&self, vm: &dyn Vm) -> EcoString {
        match self {
            Value::Int(v) => eco_format!("{v}"),
            Value::Bool(v) => eco_format!("{v}"),
            Value::Unit(_) => eco_format!("()"),
            Value::Str(v) => v.repr(vm),
            Value::Func(v) => eco_format!("{v}"),
            Value::Type(v) => eco_format!("{v}"),
            Value::Iterator(v) => eco_format!("iterator({v:?})"),
            Value::Box(v) => eco_format!("{v}"),
            Value::Array(v) => v.repr(vm),
            Value::Range(r) => r.repr(vm),
            Value::Map(m) => m.repr(vm),
            Value::Module(m) => m.name().clone(),
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

primitive!(i64: "Int", Int);
primitive!(bool: "Bool", Bool);
primitive!(Str: "Str", Str);
primitive!(Func: "Func", Func);
primitive!(Type: "Type", Type);
primitive!(UnitValue: "Unit", Unit);
primitive!(IterValue: "Iterator", Iterator);
primitive!(Boxed: "Box", Box);
primitive!(ArrayValue: "Array", Array);
primitive!(RangeValue: "Range", Range);
primitive!(MapValue: "Map", Map);
primitive!(Module: "Module", Module);
