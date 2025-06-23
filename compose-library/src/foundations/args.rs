use crate::diag::{bail, error, At, SourceDiagnostic, SourceResult, Spanned};
use crate::foundations::cast::FromValue;
use crate::foundations::IntoValue;
use crate::{Trace, Value};
use compose_library::UntypedRef;
use compose_syntax::Span;
use ecow::{eco_vec, EcoVec};

pub struct Args {
    pub span: Span,
    pub items: EcoVec<Arg>,
}

impl Args {
    pub fn new<T: IntoValue>(span: Span, items: impl IntoIterator<Item = T>) -> Self {
        let items = items
            .into_iter()
            .map(|value| Arg {
                span,
                name: None,
                value: Spanned::new(value.into_value(), span),
            })
            .collect();
        Self { span, items }
    }
    pub fn insert(&mut self, index: i32, span: Span, value: Value) {
        self.items.insert(
            index as usize,
            Arg {
                span,
                name: None,
                value: Spanned::new(value, span),
            },
        );
    }
    pub fn push(&mut self, span: Span, value: Value) {
        self.items.push(Arg {
            span,
            name: None,
            value: Spanned::new(value, span),
        });
    }

    pub fn spanned(mut self, span: Span) -> Args {
        if self.span.is_detached() {
            self.span = span;
        }
        self
    }

    pub fn eat<T>(&mut self) -> SourceResult<Option<T>>
    where
        T: FromValue<Spanned<Value>>,
    {
        for (i, item) in self.items.iter().enumerate() {
            if item.name.is_some() {
                // Only return positional args
                continue;
            }
            let value = self.items.remove(i).value;
            let span = value.span;
            return T::from_value(value).at(span).map(Some)
        }
        Ok(None)
    }

    pub fn expect<T>(&mut self, what: &str) -> SourceResult<T>
    where
        T: FromValue<Spanned<Value>>,
    {
        match self.eat()? {
            Some(v) => Ok(v),
            None => bail!(self.missing_argument(what)),
        }
    }

    /// Remove all args with the given name and return the last
    /// or an error if the conversion fails.
    ///
    /// If no such arg exists, then Ok(None) is returned
    pub fn named<T>(&mut self, name: &str) -> SourceResult<Option<T>>
    where
        T: FromValue<Spanned<Value>>,
    {
        let mut found = None;

        for i in (0..self.items.len()).rev() {
            if self.items[i].name.as_deref() == Some(name) {
                let value = self.items.remove(i).value;
                let span = value.span;
                let casted = T::from_value(value).at(span)?;
                
                if found.is_none() {
                    found = Some(casted);
                }
            }
        }

        Ok(found)
    }

    /// Find and consume all castable positional arguments
    pub fn all<T>(&mut self) -> SourceResult<Vec<T>>
    where
        T: FromValue<Spanned<Value>>,
    {
        let mut vals = Vec::new();
        let mut errors = eco_vec![];

        self.items.retain(|item| {
            if item.name.is_some() {
                return true;
            }

            let span = item.value.span;
            let spanned = Spanned::new(std::mem::take(&mut item.value.value), span);
            match T::from_value(spanned).at(span) {
                Ok(v) => vals.push(v),
                Err(e) => errors.extend(e),
            }
            false
        });

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(vals)
    }

    fn missing_argument(&self, what: &str) -> SourceDiagnostic {
        // TODO: Handle named arguments
        error!(self.span, "missing argument `{}`", what)
    }

    /// Take out all arguments into a new instance.
    pub fn take(&mut self) -> Self {
        Self {
            span: self.span,
            items: std::mem::take(&mut self.items),
        }
    }

    /// Return an "unexpected argument" error if there is any remaining
    /// argument.
    pub fn finish(self) -> SourceResult<()> {
        if let Some(arg) = self.items.first() {
            // TODO: Handle named arguments
            bail!(arg.span, "unexpected argument");
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub span: Span,
    pub name: Option<String>,
    pub value: Spanned<Value>,
}

pub struct Meta(u8);

impl Meta {
    const REF: Meta = Meta(1);
    const MUT: Meta = Meta(2);
    
    pub fn new() -> Meta {
        Meta(0)
    }

    pub fn is_ref(&self) -> bool {
        (self.0 & Meta::REF.0) > 0
    }

    pub fn is_mut(&self) -> bool {
        (self.0 & Meta::MUT.0) > 0
    }

    pub fn join(&self, other: Meta) -> Meta {
        Meta(self.0 | other.0)
    }
}

impl Trace for Args {
    fn visit_refs(&self, f: &mut dyn FnMut(UntypedRef)) {
        for arg in &self.items {
            arg.value.value.visit_refs(f);
        }
    }
}
