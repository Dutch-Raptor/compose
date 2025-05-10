/// Return an error at the given item.
macro_rules! bail {
    (callsite, $($tts:tt)*) => {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            format!("typst: {}", format!($($tts)*))
        ))
    };
    ($item:expr, $($tts:tt)*) => {
        return Err(syn::Error::new_spanned(
            &$item,
            format!("typst: {}", format!($($tts)*))
        ))
    };
}

pub(crate) use bail;
