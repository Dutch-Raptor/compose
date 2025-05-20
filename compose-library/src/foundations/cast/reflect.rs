use crate::repr::separated_list;
use compose_library::{Type, Value};
use ecow::{eco_format, EcoString};
use std::fmt::Write;

pub trait Reflect {
    /// What can be turned into this value?
    fn input() -> CastInfo;

    /// What can this value turn into?
    fn output() -> CastInfo;

    /// Whether the given value can turn into this type
    fn castable(value: &Value) -> bool;

    fn error(found: &Value) -> EcoString {
        Self::input().error(found)
    }
}

pub enum CastInfo {
    Any,
    /// Any value of a specific type
    Type(Type),
    Union(Vec<Self>),
}

impl CastInfo {
    pub fn error(&self, found: &Value) -> EcoString {
        let mut parts = vec![];

        self.walk(|info| match info {
            CastInfo::Any => parts.push("any".into()),
            CastInfo::Type(ty) => parts.push(eco_format!("{ty}")),
            CastInfo::Union(_) => {}
        });

        let mut msg = String::from("expected ");

        msg.push_str(&separated_list(&parts, "or"));

        msg.push_str(", found ");
        write!(msg, "{}", found.ty()).unwrap();

        msg.into()
    }

    pub fn walk<F>(&self, mut f: F)
    where
        F: FnMut(&Self),
    {
        fn inner<F>(info: &CastInfo, f: &mut F)
        where
            F: FnMut(&CastInfo),
        {
            match info {
                CastInfo::Union(infos) => {
                    for info in infos {
                        inner(info, f);
                    }
                }
                _ => f(info),
            }
        }

        inner(self, &mut f);
    }
}
