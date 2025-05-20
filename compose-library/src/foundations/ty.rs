use std::fmt::Display;
use compose_macros::{cast, ty};
use compose_utils::Static;


#[derive(Clone, Debug, PartialEq)]
#[ty(cast)]
pub struct Type(Static<NativeTypeData>);

impl Type {
    pub fn of<T: NativeType>() -> Self { T::ty() }
}

#[derive(Debug, PartialEq)]
pub struct NativeTypeData {
    pub name: &'static str,
    pub title: &'static str,
    pub docs: &'static str,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.name)
    }
}

pub trait NativeType {
    const NAME: &'static str;
    fn ty() -> Type { Type::from(Self::data()) }

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