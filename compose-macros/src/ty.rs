use crate::kw;
use crate::util::{
    BareType, bail, determine_name_and_title, documentation, foundations, parse_flag, parse_string,
};
use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::Result;
use syn::parse::{Parse, ParseStream};

pub fn ty(stream: TokenStream, item: &syn::Item) -> Result<TokenStream> {
    let meta: Meta = syn::parse2(stream)?;

    let bare: BareType;
    let (ident, attrs, keep) = match &item {
        syn::Item::Struct(item) => (&item.ident, &item.attrs, true),
        syn::Item::Type(item) => (&item.ident, &item.attrs, true),
        syn::Item::Enum(item) => (&item.ident, &item.attrs, true),
        syn::Item::Verbatim(item) => {
            bare = syn::parse2(item.clone())?;
            (&bare.ident, &bare.attrs, false)
        }
        _ => bail!(item, "invalid type item"),
    };

    let ty = parse(meta, ident.clone(), attrs)?;

    Ok(create(&ty, keep.then_some(item)))
}

fn create(ty: &Type, item: Option<&syn::Item>) -> TokenStream {
    let Type {
        meta,
        name,
        title,
        ident,
        docs,
    } = ty;

    let cast = (!meta.cast).then_some(quote! {
        #foundations::cast! {
            type #ident,
        }
    });
    
    let scope = if meta.scope {
        quote! { <#ident as #foundations::scope::NativeScope>::scope() }
    } else {
        quote! { &#foundations::scope::EMPTY_SCOPE }
    };

    let data = quote! {
        #foundations::types::NativeTypeData {
            name: #name,
            title: #title,
            docs: #docs,
            scope: ::std::sync::LazyLock::new(|| #scope),
        }
    };

    let attr = match item {
        Some(_) => quote! {
            #[allow(rustdoc::broken_intra_doc_links)]
        },
        None => quote! {},
    };

    quote! {
        #attr
        #item
        #cast

        impl #foundations::types::NativeType for #ident {
            const NAME: &'static str = #name;

            fn data() -> &'static #foundations::types::NativeTypeData {
                static DATA: #foundations::types::NativeTypeData = #data;
                &DATA
            }
        }
    }
}

fn parse(meta: Meta, ident: Ident, attrs: &[syn::Attribute]) -> Result<Type> {
    let docs = documentation(attrs);
    let (name, title) =
        determine_name_and_title(meta.name.clone(), meta.title.clone(), &ident, None)?;

    Ok(Type {
        meta,
        name,
        title,
        ident,
        docs,
    })
}

struct Type {
    meta: Meta,
    name: String,
    title: String,
    ident: Ident,
    docs: String,
}

pub struct Meta {
    name: Option<String>,
    title: Option<String>,
    scope: bool,
    cast: bool,
}

impl Parse for Meta {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(Self {
            scope: parse_flag::<kw::scope>(input)?,
            cast: parse_flag::<kw::cast>(input)?,
            title: parse_string::<kw::title>(input)?,
            name: parse_string::<kw::name>(input)?,
        })
    }
}
