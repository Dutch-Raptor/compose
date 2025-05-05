use ecow::EcoString;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Str(EcoString);

impl From<EcoString> for Str {
    fn from(s: EcoString) -> Self {
        Str(s)
    }
}