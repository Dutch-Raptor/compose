use compose_library::{Value};
use compose_library::diag::StrResult;
use compose_macros::{func, scope};
use crate::diag::bail;

#[func(scope)]
pub fn assert(cond: bool) -> StrResult<()> {
    if !cond {
        bail!("assertion failed")
    } else {
        Ok(())
    }
}

#[scope]
impl assert {
    #[func]
    pub fn eq(left: Value, right: Value) -> StrResult<()> {
        if left != right {
            bail!(
                "assertion `left == right` failed\n{:>7} = {left:?}\n{:>7} = {right:?}",
                "left:",
                "right:"
            )
        } else {
            Ok(())
        }
    }

    #[func]
    pub fn ne(left: Value, right: Value) -> StrResult<()> {
        if left == right {
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