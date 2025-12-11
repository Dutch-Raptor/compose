use crate::foundations::ops::Comparison;
use crate::diag::bail;
use compose_library::{Value, Vm};
use compose_library::diag::StrResult;
use compose_macros::{func, scope};
use ecow::EcoString;
use compose_library::repr::Repr;

#[func(scope)]
pub fn assert(cond: bool, #[named] message: Option<EcoString>) -> StrResult<()> {
    if !cond {
        match message {
            Some(msg) => bail!("assertion failed: {}", msg),
            None => bail!("assertion failed"),
        }
    } else {
        Ok(())
    }
}

#[scope]
impl assert {
    #[func]
    pub fn eq(vm: &dyn Vm, left: Value, right: Value, #[named] message: Option<EcoString>) -> StrResult<()> {
        
        if !left.equals(&right, vm.heap())? {
            let l_repr = left.repr(vm);
            let r_repr = right.repr(vm);
            bail!(
                "assertion `left == right` failed{}\n{:>7} = {l_repr}\n{:>7} = {r_repr}",
                message.map(|msg| format!(": {}", msg)).unwrap_or_default(),
                "left:",
                "right:"
            )
        } else {
            Ok(())
        }
    }

    #[func]
    pub fn ne(vm: &dyn Vm, left: Value, right: Value) -> StrResult<()> {
        if left.equals(&right, vm.heap())? {
            bail!(
                "assertion `left != right` failed\n{:>7} = {left:?}\n{:>7} = {right:?}",
                "left:",
                "right:"
            )
        } else {
            Ok(())
        }
    }
}
