use crate::util::{bail, foundations, BareType};
use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{parse_quote, ImplItem, ImplItemConst, ImplItemFn, ItemImpl, Result};

pub fn scope(_: TokenStream, item: syn::Item) -> Result<TokenStream> {
    let syn::Item::Impl(mut item) = item else {
        bail!(item, "Expected an impl block");
    };

    let self_ty = &item.self_ty;

    // The name of the extension trait that will add methods to primitive types.
    let primitive_ident_ext = match self_ty.as_ref() {
        syn::Type::Path(path) => match path.path.get_ident() {
            Some(ident) if is_primitive(ident) => Some(quote::format_ident!("{ident}Ext")),
            _ => None,
        },
        _ => None,
    };

    let self_ty_expr = match &primitive_ident_ext {
        None => quote! { #self_ty },
        Some(ident_ext) => quote! { <#primitive_ident_ext as #ident_ext> },
    };

    let mut definitions = Vec::new();

    for child in &mut item.items {
        let bare: BareType;

        let def = match child {
            ImplItem::Const(item) => handle_const(&self_ty_expr, item)?,
            ImplItem::Fn(item) => handle_fn(self_ty, item)?,
            ImplItem::Verbatim(item) => {
                bare = syn::parse2(item.clone())?;
                handle_type(&bare)?
            }
            _ => bail!(child, "Unexpected item in scope block"),
        };

        definitions.push(def);
    }

    let base = match &primitive_ident_ext {
        None => quote! { #item },
        Some(ident_ext) => rewrite_primitive_base(&item, ident_ext),
    };
    
    Ok(quote! {
        #base
        impl #foundations::NativeScope for #self_ty {
            fn scope() -> #foundations::Scope {
                let mut scope = #foundations::Scope::new();
                #(#definitions;)*
                scope
            }
        }
    })
}

// Rewrites the base type into an extension trait with implementations
fn rewrite_primitive_base(item: &ItemImpl, ident_ext: &Ident) -> TokenStream {
    let mut sigs = vec![];
    let mut items = vec![];
    for sub in &item.items {
        match sub.clone() {
            syn::ImplItem::Fn(mut func) => {
                func.vis = syn::Visibility::Inherited;
                items.push(func.clone());

                let mut sig = func.sig;
                let inputs = sig.inputs.iter().cloned().map(|mut input| {
                    if let syn::FnArg::Typed(typed) = &mut input {
                        typed.attrs.clear();
                    }
                    input
                });
                sig.inputs = parse_quote! { #(#inputs),* };

                let ident_data = quote::format_ident!("{}_data", sig.ident);
                sigs.push(quote! { #sig; });
                sigs.push(quote! {
                    fn #ident_data() -> &'static #foundations::NativeFuncData;
                });
            }

            syn::ImplItem::Const(cons) => {
                sigs.push(quote! { #cons });
            }

            _ => {}
        }
    }

    let self_ty = &item.self_ty;
    quote! {
        #[allow(non_camel_case_types)]
        trait #ident_ext {
            #(#sigs)*
        }

        impl #ident_ext for #self_ty {
            #(#items)*
        }
    }
}

fn handle_type(bare: &BareType) -> Result<TokenStream> {
    let ident = &bare.ident;
    Ok(quote! { scope.define_type::<#ident>() })
}

fn handle_fn(self_ty: &syn::Type, item: &mut ImplItemFn) -> Result<TokenStream> {
    let Some(attr) = item
        .attrs
        .iter_mut()
        .find(|attr| attr.meta.path().is_ident("func"))
    else {
        bail!(item, "scope function is missing #[func] attribute");
    };

    let ident_data = quote::format_ident!("{}_data", item.sig.ident);

    match &mut attr.meta {
        syn::Meta::Path(_) => {
            *attr = parse_quote! { #[func(parent = #self_ty)] };
        }
        syn::Meta::List(list) => {
            let tokens = &list.tokens;
            list.tokens = quote! { #tokens, parent = #self_ty };
        }
        syn::Meta::NameValue(_) => bail!(attr.meta, "invalid func attribute"),
    }

    Ok(quote! {
        scope.define_func_with_data(#self_ty::#ident_data())
    })
}

fn handle_const(self_ty: &TokenStream, item: &mut ImplItemConst) -> Result<TokenStream> {
    let ident = &item.ident;
    let name = ident.to_string();

    Ok(quote! { scope.define(#name, #self_ty::#ident)})
}

fn is_primitive(ident: &Ident) -> bool {
    ident == "bool" || ident == "i64" || ident == "f64"
}
