use crate::diag::{bail, SourceResult, StrResult};
use crate::foundations::args::Args;
use crate::{Sink, Value};
use compose_library::{Engine, Scope};
use compose_macros::{cast, ty};
use compose_syntax::{ast, Span, SyntaxNode};
use compose_utils::Static;
use std::fmt;
use std::sync::LazyLock;
use compose_syntax::ast::AstNode;

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

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.repr {
            Repr::Native(native) => write!(f, "{}", native.0.name),
            Repr::Closure(closure) => closure.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Repr {
    Native(Static<NativeFuncData>),
    Closure(Closure)
}

impl Func {
    pub fn call(&self, engine: &mut Engine, mut args: Args) -> SourceResult<Value> {
        match &self.repr {
            Repr::Native(native) => {
                let value = (native.closure)(engine, &mut args)?;
                args.finish()?;
                Ok(value)
            }
            Repr::Closure(closure) => {
                (engine.routines.eval_closure)(
                    self, closure, engine.world, args
                )
            }
        }
    }

    pub fn scope(&self) -> Option<&'static Scope> {
        match &self.repr {
            Repr::Native(native) => Some(&native.0.scope),
            Repr::Closure(_) => None,
        }
    }
    
    pub fn name(&self) -> Option<&str> {
        match &self.repr {
            Repr::Native(native) => Some(native.0.name),
            Repr::Closure(_) => None,
        }
    }

    pub fn field(&self, field: &str, access_span: Span, sink: &mut Sink) -> StrResult<&Value> {
        let scope = self.scope().ok_or("Cannot access fields on user-defined functions")?;
        match scope.get(field) {
            Some(binding) => Ok(binding.read_checked(access_span, sink)),
            None => match self.name() {
                Some(name) => bail!("function `{name}` does not contain field `{field}`"),
                None => bail!("Function does not contain field `{field}`"),
            }
        }
    }
    
    pub fn path(&self, path: &str, access_span: Span, sink: &mut Sink) -> StrResult<&Value> {
        let scope = self.scope().ok_or("Cannot access fields on user-defined functions")?;
        match scope.get(path) {
            Some(binding) => Ok(binding.read_checked(access_span, sink)),
            None => match self.name() {
                Some(name) => bail!("function `{name}` does not contain associated field `{path}`"),
                None => bail!("Function does not contain associated field `{path}`"),
            }
        }
    }
    
    pub fn is_associated_function(&self) -> bool {
        match self.repr {
            Repr::Native(n) => match n.fn_type {
                FuncType::Method => false,
                FuncType::Associated => true,
            }
            Repr::Closure(_) => false,
        }
    }
}

pub trait NativeFunc {
    fn data() -> &'static NativeFuncData;
}

#[derive(Debug)]
pub struct NativeFuncData {
    pub closure: fn(&mut Engine, &mut Args) -> SourceResult<Value>,
    pub name: &'static str,
    pub scope: LazyLock<Scope>,
    pub fn_type: FuncType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub node: SyntaxNode,
    pub defaults: Vec<Value>,
    pub num_pos_params: usize,
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let closure: ast::Closure = self.node.cast().expect("closure");
        let params = closure.params().to_untyped().create_text();
        
        write!(f, "{} => ...", params)
    }
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

impl From<Closure> for Func {
    fn from(closure: Closure) -> Self {
        Repr::Closure(closure).into()
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