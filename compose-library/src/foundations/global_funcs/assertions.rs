use crate::diag::bail;
use compose_library::Value;
use compose_library::diag::StrResult;
use compose_macros::{func, scope};
use ecow::EcoString;

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
    pub fn eq(left: Value, right: Value, #[named] message: Option<EcoString>) -> StrResult<()> {
        if left != right {
            bail!(
                "assertion `left == right` failed{}\n{:>7} = {left:?}\n{:>7} = {right:?}",
                message.map(|msg| format!(": {}", msg)).unwrap_or_default(),
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
