use std::num::NonZeroU64;

pub trait Id {
    fn from_nonzero_u64(id: NonZeroU64) -> Self;
}

impl<T: From<NonZeroU64>> Id for T {
    fn from_nonzero_u64(id: NonZeroU64) -> Self {
        id.into()
    }
}

#[derive(Debug, Clone)]
pub struct IdStore<I: Id> {
    values: NonZeroU64,
    phantom: std::marker::PhantomData<I>,
}

impl<I: Id> IdStore<I> {
    pub fn new() -> Self {
        Self {
            values: NonZeroU64::new(1).expect("IdStore must be non-zero"),
            phantom: std::marker::PhantomData,
        }
    }

    pub fn next(&mut self) -> I {
        let id = self.values;
        self.values = self.values.saturating_add(1);
        I::from_nonzero_u64(id)
    }

    pub fn current(&self) -> I {
        I::from_nonzero_u64(self.values)
    }
}
