use compose_macros::{cast, ty};
use crate::diag::SourceResult;
use crate::foundations::args::Args;
use crate::Value;
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
}

pub trait NativeFunc {
    fn data() -> &'static NativeFuncData;
}

#[derive(Debug)]
pub struct NativeFuncData {
    pub closure: fn(&mut Args) -> SourceResult<Value>,
    pub name: &'static str,
    // pub params: LazyLock<Vec<ParamInfo>>
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