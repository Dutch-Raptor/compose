use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{parse_quote, FnArg, ItemFn, Pat, Result, ReturnType};
use syn::token::Token;
use crate::util::bail;

pub fn func(stream: TokenStream, item: &syn::ItemFn) -> Result<TokenStream> {
    let func = parse(stream, item)?;
    Ok(create(&func, item))
}

fn create(func: &Func, item: &ItemFn) -> TokenStream {
    let Func { rust_name, .. } = func;
    let definition = item;

    // Create a type to which the func data will be attached
    let function_type = create_func_ty(func);
    let data = create_func_data(func);
    
    let data_impl = quote! {
        impl ::compose_library::foundations::NativeFunc for #rust_name {
            fn data() -> &'static ::compose_library::foundations::NativeFuncData {
                static DATA: ::compose_library::foundations::NativeFuncData = #data;
                &DATA
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
        params,
        return_type,
    } = func;

    let closure = create_wrapper_closure(func);
    
    let name = quote! { #name };
    
    quote! {
        ::compose_library::foundations::NativeFuncData {
            name: #name,
            closure: #closure,
        }
    }
}

fn create_wrapper_closure(func: &Func) -> TokenStream {
    let arg_handlers = {
        let args = func.params.iter()
            .map(create_param_parser);
        
        quote! {
            #(#args)*
        }
    };
    
    let finish = Some(quote! {args.take().finish()?;  });
    
    let call = {
        let forwarded = func.params.iter()
            .map(bind);
        
        quote! {
            __func(#(#forwarded,)*)
        }
    };
    
    let ident = &func.rust_name;
    quote! {
        |args| {
            let __func = #ident;
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
    let Param { ident, ty, name, .. } = param;
    
    let value = quote! {
        args.expect(#name)?
    };
    
    quote! {
        let mut #ident: #ty = #value;
    }
}

fn create_func_ty(func: &Func) -> TokenStream {
    let ident = &func.rust_name;
    quote! {
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        pub enum #ident {}
    }
}

struct Func {
    /// The function name as exposed to `Compose`
    name: String,
    /// The rust name of this function
    rust_name: Ident,
    params: Vec<Param>,
    return_type: syn::Type,
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
    let name = determine_name(&item.sig.ident);

    let mut params = Vec::new();
    for input_param in &item.sig.inputs {
        parse_param(&mut params, input_param)?;
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
    })
}

fn parse_param(params: &mut Vec<Param>, input: &FnArg) -> Result<()> {
    let typed = match input {
        FnArg::Receiver(_) => bail!(input, "receiver is not supported yet"),
        FnArg::Typed(typed) => typed,
    };

    let ident = match typed.pat.as_ref() {
        syn::Pat::Ident(syn::PatIdent { ident, .. }) => ident,
        _ => bail!(typed.pat, "expected identifier. Destructuring is not supported"),
    };

    params.push(Param {
        binding: Binding::Owned,
        ident: ident.clone(),
        ty: *typed.ty.clone(),
        name: ident.to_string(),
    });

    Ok(())
}

fn determine_name(ident: &Ident) -> String {
    let name = ident.to_string();
    name
}
