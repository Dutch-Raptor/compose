use compose_doc::ErrorHandlingMode;
use proc_macro::TokenStream as BoundaryStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{Attribute, Item, parse_macro_input};
use unindent::unindent;

#[proc_macro]
pub fn compose_doc(input: BoundaryStream) -> BoundaryStream {
    let ComposeDocInput { attrs, item } = parse_macro_input!(input as ComposeDocInput);

    let lines: Vec<_> = attrs.iter().filter_map(extract_doc_line).collect();

    let markdown = unindent(&lines.join("\n"));

    // Transform markdown
    let transformed = match compose_doc::transform_markdown(
        &markdown,
        &compose_doc::Config::new()
            .with_no_color()
            .with_code_block_error_mode(ErrorHandlingMode::EmitAsTests)
            .with_output_block_error_mode(ErrorHandlingMode::EmitAsTests),

    ) {
        Ok(t) => t,
        Err(e) => {
            return syn::Error::new_spanned(&item, format!("{}: {}", e.line, e.message))
                .to_compile_error()
                .into();
        }
    };

    let doc_lines = transformed.lines().map(|line| quote! { #[doc = #line] });

    // Attach transformed doc attributes to the item
    let output = quote! {
        #(#doc_lines)*
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
        #item
    };

    output.into()
}

struct ComposeDocInput {
    attrs: Vec<Attribute>,
    item: Item,
}

impl syn::parse::Parse for ComposeDocInput {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let mut item: Item = input.parse()?;
        let attrs = std::mem::take(match &mut item {
            Item::Fn(item) => &mut item.attrs,
            Item::Mod(item) => &mut item.attrs,
            Item::Trait(item) => &mut item.attrs,
            Item::Const(item) => &mut item.attrs,
            Item::Enum(item) => &mut item.attrs,
            Item::Struct(item) => &mut item.attrs,
            _ => {
                return Err(syn::Error::new(
                    item.span(),
                    "Only functions, modules, traits, constants, enums and structs are supported",
                ));
            }
        });
        Ok(Self { attrs, item })
    }
}

fn extract_doc_line(attr: &Attribute) -> Option<String> {
    let syn::Meta::NameValue(meta) = &attr.meta else {
        return None;
    };
    if !(meta.path.is_ident("doc")) {
        return None;
    }
    let syn::Expr::Lit(lit) = &meta.value else {
        return None;
    };
    let syn::Lit::Str(string) = &lit.lit else {
        return None;
    };

    let full = string.value();
    Some(full)
}
