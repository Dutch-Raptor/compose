use crate::{Scope, Sink, UnBoundError, Value};
use compose_library::UnboundItem;
use compose_library::diag::StrResult;
use compose_macros::{cast, ty};
use compose_syntax::Span;
use compose_utils::Static;
use ecow::eco_format;
use std::fmt::Display;
use std::sync::LazyLock;

#[derive(Clone, Debug, PartialEq)]
#[ty(cast, name = "type")]
pub struct Type(Static<NativeTypeData>);

impl Type {
    pub fn of<T: NativeType>() -> Self {
        T::ty()
    }

    pub fn scope(&self) -> &'static Scope {
        &self.0.0.scope
    }

    pub fn field(&self, field: &str, access_span: Span, sink: &mut impl Sink) -> StrResult<&Value> {
        self.0
            .scope
            .get(field)
            .map(|b| b.read_checked(access_span, sink))
            .ok_or_else(|| eco_format!("no field or method `{}` on type `{}`", field, self))
    }

    pub fn path(
        &self,
        path: &str,
        access_span: Span,
        sink: &mut impl Sink,
    ) -> Result<&Value, UnBoundError> {
        self.0
            .scope
            .try_get(path)
            .map_err(|e| e.with_item(UnboundItem::AssociatedFieldOrFunction(self.name().into())))
            .map(|b| b.read_checked(access_span, sink))
    }

    pub fn name(&self) -> &'static str {
        self.0.name
    }
}

#[derive(Debug)]
pub struct NativeTypeData {
    pub name: &'static str,
    pub title: &'static str,
    pub docs: &'static str,
    pub scope: LazyLock<Scope>,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.name)
    }
}

pub trait NativeType {
    const NAME: &'static str;
    fn ty() -> Type {
        Type::from(Self::data())
    }

    fn data() -> &'static NativeTypeData;
}

impl From<&'static NativeTypeData> for Type {
    fn from(data: &'static NativeTypeData) -> Self {
        Self(Static(data))
    }
}

cast! {
    &'static NativeTypeData,
    self => Type::from(self).into_value(),
}
