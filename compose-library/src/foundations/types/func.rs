use crate::diag::{SourceResult, StrResult, bail};
use crate::foundations::args::Args;
use crate::vm::Vm;
use compose_error_codes::E0010_UNCAPTURED_VARIABLE;
use compose_library::diag::{Spanned, error};
use compose_macros::{cast, ty};
use compose_syntax::ast::{AstNode};
use compose_syntax::{Label, Span, SyntaxNode, ast};
use compose_utils::Static;
use ecow::{EcoString, eco_format, eco_vec};
use std::collections::HashMap;
use std::fmt;
use std::sync::LazyLock;
use tap::Tap;
use compose_library::foundations::scope::Scope;
use compose_library::gc::{Trace, UntypedRef};
use compose_library::sink::Sink;
use compose_library::Value;

#[derive(Clone, Debug, PartialEq)]
#[ty(cast)]
pub struct Func {
    pub kind: FuncKind,
    pub span: Span,
}

impl Func {
    pub(crate) fn spanned(mut self, span: Span) -> Func {
        if self.span.is_detached() {
            self.span = span;
        }
        self
    }

    pub(crate) fn named(mut self, name: Spanned<EcoString>) -> Func {
        if let FuncKind::Closure(closure) = &mut self.kind {
            closure.unresolved_captures.remove(&name.value);
            closure.name = Some(name);
        }
        self
    }

    pub(crate) fn resolve(&self) -> SourceResult<()> {
        match &self.kind {
            FuncKind::Closure(closure) => {
                closure.resolve_captures()?;
            }
            FuncKind::Native(_) => {}
        }
        Ok(())
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            FuncKind::Native(native) => write!(f, "{}", native.0.name),
            FuncKind::Closure(closure) => closure.fmt(f),
        }
    }
}

impl Trace for Func {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        match &self.kind {
            FuncKind::Native(native) => {
                native.scope.visit_refs(f);
            }
            FuncKind::Closure(closure) => {
                closure.captured.visit_refs(f);
                closure.defaults.iter().for_each(|v| v.visit_refs(f));
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FuncKind {
    Native(Static<NativeFuncData>),
    Closure(Box<Closure>),
}

impl Func {
    pub fn call(&self, vm: &mut dyn Vm, args: Args) -> SourceResult<Value> {
        vm.call_func(self, args)
    }

    pub fn scope(&self) -> Option<&'static Scope> {
        match &self.kind {
            FuncKind::Native(native) => Some(&native.0.scope),
            FuncKind::Closure(_) => None,
        }
    }

    pub fn name(&self) -> Option<&str> {
        match &self.kind {
            FuncKind::Native(native) => Some(native.0.name),
            FuncKind::Closure(_) => None,
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
        match self.kind {
            FuncKind::Native(n) => match n.fn_type {
                FuncType::Method => false,
                FuncType::MethodMut => false,
                FuncType::Associated => true,
            },
            FuncKind::Closure(_) => false,
        }
    }

    pub fn requires_mut_self(&self) -> bool {
        match self.kind {
            FuncKind::Native(n) => match n.fn_type {
                FuncType::Method => false,
                FuncType::MethodMut => true,
                FuncType::Associated => false,
            },
            FuncKind::Closure(_) => false,
        }
    }
}

pub trait NativeFunc {
    fn data() -> &'static NativeFuncData;
}

#[derive(Debug)]
pub struct NativeFuncData {
    pub closure: fn(&mut dyn Vm, &mut Args) -> SourceResult<Value>,
    pub name: &'static str,
    pub scope: LazyLock<&'static Scope>,
    pub fn_type: FuncType,
}

impl NativeFuncData {
    pub fn call(&self, vm: &mut dyn Vm, mut args: Args) -> SourceResult<Value> {
        (self.closure)(vm, &mut args)
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
    pub fn resolve_captures(&self) -> SourceResult<()> {
        if !self.unresolved_captures.is_empty() {
            let mut captures = self.unresolved_captures.iter();

            let names = self
                .unresolved_captures
                .keys()
                .map(|name| name.trim())
                .collect::<Vec<_>>()
                .tap_mut(|v| v.sort_unstable())
                .join(", ");

            let params = self
                .node
                .cast::<ast::Lambda>()
                .expect("Closure contains non lambda node")
                .params()
                .children()
                .map(|p| p.to_untyped().to_text())
                .collect::<Vec<_>>()
                .join(", ");

            let (first_name, first_span) = captures.next().expect("unresolved captures was checked to be non-empty");

            let mut err = error!(*first_span, "closure uses outer variables that are not captured";
                label_message: "outer variable `{first_name}` used here";
                hint: "explicitly capture them by adding them to a capture list: `{{ |{names}| {params}=> ... }}`";
                code: &E0010_UNCAPTURED_VARIABLE
            );

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

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let closure: ast::Lambda = self.node.cast().expect("closure");
        let params = closure.params().to_untyped().to_text();

        write!(f, "{} => ...", params)
    }
}

#[derive(Debug)]
pub enum FuncType {
    Method,
    MethodMut,
    Associated,
}

impl From<FuncKind> for Func {
    fn from(value: FuncKind) -> Self {
        Self {
            span: Span::detached(),
            kind: value,
        }
    }
}

impl From<&'static NativeFuncData> for Func {
    fn from(data: &'static NativeFuncData) -> Self {
        FuncKind::Native(Static(data)).into()
    }
}

impl From<Closure> for Func {
    fn from(closure: Closure) -> Self {
        FuncKind::Closure(Box::new(closure)).into()
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
