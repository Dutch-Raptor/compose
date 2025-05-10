use crate::diag::{At, SourceDiagnostic, SourceResult, Spanned, bail, error};
use crate::foundations::cast::FromValue;
use crate::{IntoValue, Value};
use compose_syntax::Span;
use ecow::EcoVec;

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
                value: Spanned::new(value.into_value(), span),
            })
            .collect();
        Self { span, items }
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
        // TODO: Handle named args;

        if self.items.is_empty() {
            return Ok(None);
        }

        let value = self.items.remove(0).value;
        let span = value.span;
        return T::from_value(value).at(span).map(Some);
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
    pub value: Spanned<Value>,
}
