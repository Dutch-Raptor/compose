use crate::diag::{SourceResult, StrResult, bail};
use crate::foundations::args::Args;
use crate::{Sink, Value};
use compose_library::diag::{Spanned, error};
use compose_library::{Engine, Scope};
use compose_macros::{cast, ty};
use compose_syntax::ast::AstNode;
use compose_syntax::{Label, Span, SyntaxNode, ast};
use compose_utils::Static;
use dumpster::{Trace, Visitor};
use ecow::{EcoString, eco_format, eco_vec};
use std::collections::HashMap;
use std::fmt;
use std::sync::LazyLock;

#[derive(Clone, Debug, PartialEq)]
#[ty(cast)]
pub struct Func {
    repr: Repr,
    span: Span,
}

unsafe impl Trace for Func {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        self.repr.accept(visitor)?;

        Ok(())
    }
}

impl Func {
    pub(crate) fn spanned(mut self, span: Span) -> Func {
        if self.span.is_detached() {
            self.span = span;
        }
        self
    }

    pub(crate) fn named(mut self, name: Spanned<EcoString>) -> Func {
        if let Repr::Closure(closure) = &mut self.repr {
            closure.unresolved_captures.remove(&name.value);
            closure.name = Some(name);
        }
        self
    }

    pub(crate) fn resolve(&self) -> SourceResult<()> {
        if let Repr::Closure(closure) = &self.repr {
            closure.resolve()?;
        }
        Ok(())
    }

    pub(crate) fn span(&self) -> Span {
        self.span
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
    Closure(Closure),
}

unsafe impl Trace for Repr {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            Repr::Native(native) => native.0.accept(visitor),
            Repr::Closure(closure) => closure.accept(visitor),
        }
    }
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
                (engine.routines.eval_closure)(self, closure, engine.world, args)
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
        let scope = self
            .scope()
            .ok_or("Cannot access fields on user-defined functions")?;
        match scope.get(field) {
            Some(binding) => Ok(binding.read_checked(access_span, sink)),
            None => match self.name() {
                Some(name) => bail!("function `{name}` does not contain field `{field}`"),
                None => bail!("Function does not contain field `{field}`"),
            },
        }
    }

    pub fn path(&self, path: &str, access_span: Span, sink: &mut Sink) -> StrResult<&Value> {
        let scope = self
            .scope()
            .ok_or("Cannot access fields on user-defined functions")?;
        match scope.get(path) {
            Some(binding) => Ok(binding.read_checked(access_span, sink)),
            None => match self.name() {
                Some(name) => bail!("function `{name}` does not contain associated field `{path}`"),
                None => bail!("Function does not contain associated field `{path}`"),
            },
        }
    }

    pub fn is_associated_function(&self) -> bool {
        match self.repr {
            Repr::Native(n) => match n.fn_type {
                FuncType::Method => false,
                FuncType::Associated => true,
            },
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
    pub scope: LazyLock<&'static Scope>,
    pub fn_type: FuncType,
}

unsafe impl Trace for NativeFuncData {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        // Safety:
        // - `fn` type is just a pointer, does not contain any Gc<T>
        // - `&'static str`, same as above
        // - We delegate to the `Scope` `trace` impl
        // - `FnType` is a simple enum, does not contain any Gc<T>
        self.scope.accept(visitor)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub node: SyntaxNode,
    pub defaults: Vec<Value>,
    pub num_pos_params: usize,
    pub name: Option<Spanned<EcoString>>,
    pub captured: Scope,
    pub unresolved_captures: HashMap<EcoString, Span>,
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl Closure {
    pub fn resolve(&self) -> SourceResult<()> {
        if !self.unresolved_captures.is_empty() {
            let mut captures = self.unresolved_captures.iter();
            
            let names = self
                .unresolved_captures
                .keys()
                .map(|name| name.trim())
                .collect::<Vec<_>>()
                .join(", ");
            
            let params = self
                .node
                .cast::<ast::Closure>()
                .expect("Closure contains non closure node")
                .params()
                .children()
                .map(|p| p.to_untyped().to_text())
                .collect::<Vec<_>>()
                .join(", ");
            
            let (first_name, first_span) = captures.next().unwrap();

            let mut err = error!(*first_span, "outer variables used in closure but not captured";
                label_message: "outer variable `{first_name}` used here";
                hint: "explicitly capture them by adding them to a capture group: `|{names}| ({params}) => ...`");

            for (name, span) in captures {
                err = err.with_label(Label::primary(
                    *span,
                    eco_format!("outer variable `{name}` used here"),
                ));
            }

            return Err(eco_vec!(err));
        }
        Ok(())
    }
}

unsafe impl Trace for Closure {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        // Safety: Only defaults may contain Gc<T>
        self.defaults.accept(visitor)?;

        Ok(())
    }
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let closure: ast::Closure = self.node.cast().expect("closure");
        let params = closure.params().to_untyped().to_text();

        write!(f, "{} => ...", params)
    }
}

#[derive(Debug, Trace)]
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
