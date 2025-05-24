use crate::kw;
use crate::util::{bail, foundations, parse_flag, parse_key_value, parse_string};
use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_quote, FnArg, ItemFn, Result, ReturnType, Type};

pub fn func(stream: TokenStream, item: &ItemFn) -> Result<TokenStream> {
    let func = parse(stream, item)?;
    Ok(create(&func, item))
}

fn create(func: &Func, item: &ItemFn) -> TokenStream {
    let Func { rust_name, vis, .. } = func;
    let definition = item;

    // Create a type to which the func data will be attached
    let function_type = create_func_ty(func);
    let data = create_func_data(func);

    let data_impl = match function_type {
        Some(_) => quote! {
            impl ::compose_library::foundations::NativeFunc for #rust_name {
                fn data() -> &'static ::compose_library::foundations::NativeFuncData {
                    static DATA: ::compose_library::foundations::NativeFuncData = #data;
                    &DATA
                }
            }
        },
        None => {
            let ident_data = quote::format_ident!("{rust_name}_data");
            quote! {
                #vis fn #ident_data() -> &'static #foundations::NativeFuncData {
                    static DATA: #foundations::NativeFuncData = #data;
                    &DATA
                }
            }
        }
    };

    quote! {
        #definition
        #function_type
        #data_impl
    }
}

fn create_func_data(func: &Func) -> TokenStream {
    let Func {
        name,
        rust_name,
        params: _params,
        return_type: _return_type,
        scope,
        parent: _parent,
        vis: _vis,
        special,
    } = func;

    let scope = if *scope {
        quote! { <#rust_name as #foundations::NativeScope>::scope() }
    } else {
        quote! { #foundations::Scope::default() }
    };

    let closure = create_wrapper_closure(func);
    
    let fn_type = if special.self_.is_some() {
        quote! { #foundations::FuncType::Method }
    } else {
        quote! { #foundations::FuncType::Associated }       
    };

    let name = quote! { #name };

    quote! {
        ::compose_library::foundations::NativeFuncData {
            name: #name,
            closure: #closure,
            scope: ::std::sync::LazyLock::new(|| #scope),
            fn_type: #fn_type
        }
    }
}

fn create_wrapper_closure(func: &Func) -> TokenStream {
    let arg_handlers = {
        let args = func.params.iter().map(create_param_parser);

        let self_handler = func
            .special
            .self_
            .as_ref()
            .map(|param| create_param_parser(param));

        quote! {
            #self_handler
            #(#args)*
        }
    };

    let finish = Some(quote! {args.take().finish()?;  });

    let call = {
        let self_ = func
            .special
            .self_
            .as_ref()
            .map(bind)
            .map(|tokens| quote! { #tokens, });
        let forwarded = func.params.iter().map(bind);

        quote! {
            __func(#self_ #(#forwarded,)*)
        }
    };

    let parent = func.parent.as_ref().map(|ty| quote! { #ty:: });
    let ident = &func.rust_name;
    quote! {
        |args| {
            let __func = #parent #ident;
            #arg_handlers
            #finish
            let ret = #call;
            ::compose_library::foundations::IntoResult::into_result(ret, args.span)
        }
    }
}

fn bind(param: &Param) -> TokenStream {
    let Param { ident, binding, .. } = param;
    match binding {
        Binding::Owned => quote! { #ident },
        Binding::Ref => quote! { &#ident },
        Binding::RefMut => quote! { &mut #ident },
    }
}

fn create_param_parser(param: &Param) -> TokenStream {
    let Param {
        ident, ty, name, ..
    } = param;

    let value = quote! {
        args.expect(#name)?
    };

    quote! {
        let mut #ident: #ty = #value;
    }
}

fn create_func_ty(func: &Func) -> Option<TokenStream> {
    if func.parent.is_some() {
        return None;
    }
    let ident = &func.rust_name;
    Some(quote! {
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        pub enum #ident {}
    })
}

struct Func {
    /// The function name as exposed to `Compose`
    name: String,
    /// The rust name of this function
    rust_name: Ident,
    params: Vec<Param>,
    return_type: Type,
    scope: bool,
    parent: Option<Type>,
    vis: syn::Visibility,
    special: SpecialParams,
}

struct Param {
    binding: Binding,
    ident: Ident,
    ty: syn::Type,
    /// The name of the param as defined in Compose
    name: String,
}

enum Binding {
    Owned,
    Ref,
    RefMut,
}

fn parse(stream: TokenStream, item: &syn::ItemFn) -> Result<Func> {
    let meta: Meta = syn::parse2(stream)?;
    let name = determine_name(&item.sig.ident, meta.name);

    let mut params = Vec::new();
    let mut special = SpecialParams::default();
    for input_param in &item.sig.inputs {
        parse_param(&mut special, &mut params, meta.parent.as_ref(), input_param)?;
    }

    if meta.parent.is_some() && meta.scope {
        bail!(item, "A function in a scope cannot have a scope")
    }

    let return_type = match &item.sig.output {
        ReturnType::Default => parse_quote! { () },
        ReturnType::Type(_, ty) => *ty.clone(),
    };

    let rust_name = item.sig.ident.clone();

    Ok(Func {
        name,
        rust_name,
        params,
        return_type,
        special,
        scope: meta.scope,
        parent: meta.parent,
        vis: item.vis.clone(),
    })
}

/// The `..` in `#[func(..)]`.
pub struct Meta {
    /// Whether this function has an associated scope defined by the `#[scope]` macro.
    pub scope: bool,
    /// The function's name as exposed to Typst.
    pub name: Option<String>,
    /// The parent type of this function.
    ///
    /// Used for functions in a scope.
    pub parent: Option<syn::Type>,
}

impl Parse for Meta {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            scope: parse_flag::<kw::scope>(input)?,
            name: parse_string::<kw::name>(input)?,
            parent: parse_key_value::<kw::parent, _>(input)?,
        })
    }
}

#[derive(Default)]
struct SpecialParams {
    self_: Option<Param>,
}

fn parse_param(
    special_params: &mut SpecialParams,
    params: &mut Vec<Param>,
    parent: Option<&Type>,
    input: &FnArg,
) -> Result<()> {
    let typed = match input {
        FnArg::Receiver(recv) => {
            let binding = match (&recv.reference, &recv.mutability) {
                (None, None) => Binding::Owned,
                (Some(_), None) => Binding::Ref,
                (_, Some(_)) => Binding::RefMut,
            };

            special_params.self_ = Some(Param {
                binding,
                ident: syn::Ident::new("self_", recv.self_token.span()),
                ty: match parent {
                    Some(ty) => ty.clone(),
                    None => bail!(
                        recv,
                        "explicit parent type is required for functions with a self parameter"
                    ),
                },
                name: "self".to_string(),
            });
            return Ok(());
        }
        FnArg::Typed(typed) => typed,
    };

    let ident = match typed.pat.as_ref() {
        syn::Pat::Ident(syn::PatIdent { ident, .. }) => ident,
        _ => bail!(
            typed.pat,
            "expected identifier. Destructuring is not supported"
        ),
    };

    params.push(Param {
        binding: Binding::Owned,
        ident: ident.clone(),
        ty: *typed.ty.clone(),
        name: ident.to_string(),
    });

    Ok(())
}

fn determine_name(ident: &Ident, name: Option<String>) -> String {
    name.unwrap_or_else(|| ident.to_string())
}
