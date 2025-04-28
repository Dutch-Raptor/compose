use std::marker::PhantomData;

pub struct Vm<'a> {
    phantom_data: PhantomData<&'a ()>   
}

impl<'a> Vm<'a> {
    pub fn empty() -> Self {
        Self {
            phantom_data: PhantomData
        }
    }
}