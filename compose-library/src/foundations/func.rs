use std::sync::LazyLock;
use compose_library::Scope;
use compose_macros::{cast, ty};
use crate::diag::{bail, SourceResult, StrResult};
use crate::foundations::args::Args;
use crate::{Sink, Value};
use compose_syntax::Span;
use compose_utils::Static;

#[derive(Clone, Debug, PartialEq)]
#[ty(cast)]
pub struct Func {
    repr: Repr,
    span: Span,
}

impl Func {
    pub(crate) fn spanned(mut self, span: Span) -> Func {
        if self.span.is_detached() {
            self.span = span;
        }
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Repr {
    Native(Static<NativeFuncData>),
}

impl Func {
    pub fn call(&self, mut args: Args) -> SourceResult<Value> {
        match self.repr {
            Repr::Native(native) => {
                let value = (native.closure)(&mut args)?;
                args.finish()?;
                Ok(value)
            }
        }
    }

    pub fn scope(&self) -> Option<&'static Scope> {
        match &self.repr {
            Repr::Native(native) => Some(&native.0.scope)
        }
    }
    
    pub fn name(&self) -> Option<&str> {
        match &self.repr {
            Repr::Native(native) => Some(native.0.name)
        }
    }

    pub fn field(&self, field: &str, access_span: Span, sink: &mut impl Sink) -> StrResult<&Value> {
        let scope = self.scope().ok_or("Cannot access fields on user-defined functions")?;
        match scope.get(field) {
            Some(binding) => Ok(binding.read_checked(access_span, sink)),
            None => match self.name() {
                Some(name) => bail!("function `{name}` does not contain field `{field}`"),
                None => bail!("Function does not contain field `{field}`"),
            }
        }
    }
    
    pub fn is_associated_function(&self) -> bool {
        match self.repr {
            Repr::Native(n) => match n.fn_type {
                FuncType::Method => false,
                FuncType::Associated => true,
            }
        }
    }
}

pub trait NativeFunc {
    fn data() -> &'static NativeFuncData;
}

#[derive(Debug)]
pub struct NativeFuncData {
    pub closure: fn(&mut Args) -> SourceResult<Value>,
    pub name: &'static str,
    pub scope: LazyLock<Scope>,
    pub fn_type: FuncType,
}

#[derive(Debug)]
pub enum FuncType {
    Method,
    Associated,
}

impl From<Repr> for Func {
    fn from(value: Repr) -> Self {
        Self {
            span: Span::detached(),
            repr: value,
        }
    }
}

impl From<&'static NativeFuncData> for Func {
    fn from(data: &'static NativeFuncData) -> Self {
        Repr::Native(Static(data)).into()
    }
}


#[derive(Debug)]
pub struct ParamInfo {
    pub name: &'static str,
}


cast! {
    &'static NativeFuncData,
    self => Func::from(self).into_value(),
}