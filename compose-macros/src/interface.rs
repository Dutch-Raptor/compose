use crate::kw;
use crate::util::{determine_name_and_title, foundations, parse_string};
use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{ItemTrait, Result};

pub fn interface(stream: TokenStream, item: &ItemTrait) -> Result<TokenStream> {
    let meta: Meta = syn::parse2(stream)?;
    let (name, _) = determine_name_and_title(meta.name, None, &item.ident, None)?;
    let ident = &item.ident;

    Ok(quote! {
        #item

        impl #foundations::type_info::NativeInterface for dyn #ident {
            const NAME: &'static str = #name;
        }
    })
}

struct Meta {
    name: Option<String>,
}

impl Parse for Meta {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(Self {
            name: parse_string::<kw::name>(input)?,
        })
    }
}
