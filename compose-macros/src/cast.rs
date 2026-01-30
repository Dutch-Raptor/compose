use crate::util::foundations;
use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{Result, Token};

pub fn cast(stream: TokenStream) -> Result<TokenStream> {
    let input: CastData = syn::parse2(stream)?;
    let ty = &input.ty;

    // Create Reflect Impl
    let castable_body = create_castable_body(&input);
    let input_body = create_input_body(&input);
    let output_body = create_output_body();

    let reflect_impl = (!input.from_value.is_empty()).then_some({
        quote! {
            impl #foundations::cast::Reflect for #ty {
                fn castable(value: &#foundations::Value) -> bool {
                    #castable_body
                }

                fn input() -> #foundations::cast::CastInfo {
                    #input_body
                }

                fn output() -> #foundations::cast::CastInfo {
                    #output_body
                }
            }
        }
    });

    let into_value = input.into_value.map(|expr| {
        quote! {
            impl #foundations::cast::IntoValue for #ty {
                fn into_value(self) -> #foundations::Value {
                    #expr
                }
            }
        }
    });

    let from_value = (!input.from_value.is_empty()).then_some({
        let cast_checks = input.from_value.iter().map(|cast| {
            let pattern = &cast.pattern;
            let ty = &cast.ty;
            let expr = &cast.expr;

            quote! {
                if <#ty as #foundations::cast::Reflect>::castable(&value) {
                    let #pattern = <#ty as #foundations::cast::FromValue>::from_value(value)?;
                    return Ok(#expr);
                }
            }
        });

        quote! {
            impl #foundations::cast::FromValue for #ty {
                fn from_value(value: #foundations::Value) -> ::compose_library::diag::StrResult<Self> {
                    #(#cast_checks)*
                    Err(<Self as #foundations::cast::Reflect>::error(&value))
                }
            }
        }
    });

    Ok(quote! {
        #reflect_impl
        #into_value
        #from_value
    })
}

fn create_output_body() -> TokenStream {
    quote! {
        <Self as #foundations::cast::Reflect>::input()
    }
}

fn create_input_body(input: &CastData) -> TokenStream {
    let infos = input.from_value.iter().map(|cast| {
        let ty = &cast.ty;
        quote! {
            <#ty as #foundations::cast::Reflect>::input()
        }
    });

    quote! {
        #(#infos)+*
    }
}

fn create_castable_body(input: &CastData) -> TokenStream {
    let casts = input.from_value.iter().map(|cast| {
        let ty = &cast.ty;
        quote! {
            if <#ty as #foundations::cast::Reflect>::castable(&value) {
                return true;
            }
        }
    });

    quote! {
        #(#casts)*
        false
    }
}

impl Parse for CastData {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let ty = input.parse()?;
        let _: Token![,] = input.parse()?;

        let mut into_value = None;
        if input.peek(Token![self]) {
            let _: Token![self] = input.parse()?;
            let _: Token![ => ] = input.parse()?;
            into_value = Some(input.parse()?);
            let _: Token![, ] = input.parse()?;
        }

        let from_value = Punctuated::parse_terminated(input)?;

        Ok(Self {
            ty,
            into_value,
            from_value,
        })
    }
}

struct CastData {
    ty: syn::Type,
    into_value: Option<syn::Expr>,
    from_value: Punctuated<Cast, Token![,]>,
}

struct Cast {
    attrs: Vec<syn::Attribute>,
    pattern: syn::Pat,
    ty: syn::Type,
    expr: syn::Expr,
}

impl Parse for Cast {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let attrs = input.call(syn::Attribute::parse_outer)?;

        // Pat: Ty
        let pattern = syn::Pat::parse_single(input)?;
        let _: Token![:] = input.parse()?;
        let ty = input.parse()?;

        let _: Token![=>] = input.parse()?;
        let expr = input.parse()?;

        Ok(Self {
            attrs,
            pattern,
            ty,
            expr,
        })
    }
}
