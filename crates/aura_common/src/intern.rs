use string_interner::{DefaultBackend, DefaultSymbol, StringInterner};

pub type Symbol = DefaultSymbol;

#[derive(Debug, Default)]
pub struct Interner {
    inner: StringInterner<DefaultBackend>,
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        self.inner.get_or_intern(s)
    }

    pub fn resolve(&self, sym: Symbol) -> Option<&str> {
        self.inner.resolve(sym)
    }
}
