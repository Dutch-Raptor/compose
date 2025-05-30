#![allow(unused)]
mod cast;
mod func;
mod kw;
mod scope;
mod ty;
mod util;

use proc_macro::TokenStream as BoundaryStream;

#[proc_macro_attribute]
pub fn func(stream: BoundaryStream, item: BoundaryStream) -> BoundaryStream {
    let item = syn::parse_macro_input!(item as syn::ItemFn);
    func::func(stream.into(), &item)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro]
pub fn cast(stream: BoundaryStream) -> BoundaryStream {
    cast::cast(stream.into())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro_attribute]
pub fn ty(stream: BoundaryStream, item: BoundaryStream) -> BoundaryStream {
    let item = syn::parse_macro_input!(item as syn::Item);
    ty::ty(stream.into(), item)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro_attribute]
pub fn scope(stream: BoundaryStream, item: BoundaryStream) -> BoundaryStream {
    let item = syn::parse_macro_input!(item as syn::Item);
    scope::scope(stream.into(), item)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
