use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::LazyLock;
use std::time::Instant;

/// Global toggle to enable or disable tracing.
pub static ENABLE_TRACE: LazyLock<AtomicBool> = LazyLock::new(|| AtomicBool::new(false));

// Per-thread indentation tracking.
thread_local! {
    static INDENT: RefCell<usize> = RefCell::new(0);
}

static CALL_ID_COUNTER: AtomicUsize = AtomicUsize::new(1);

/// Run a closure with the current indentation level.
pub fn with_indent<F: FnOnce(usize) -> String>(f: F) {
    INDENT.with(|level| {
        let indent = *level.borrow();
        println!("{}", f(indent));
    });
}

/// Increase indentation (on function entry).
fn indent_inc() {
    INDENT.with(|i| *i.borrow_mut() += 1);
}

/// Decrease indentation (on function exit).
fn indent_dec() {
    INDENT.with(|i| *i.borrow_mut() -= 1);
}

/// Guard that logs entry/exit of a function.
pub struct TraceFnGuard {
    name: &'static str,
    enabled: bool,
    start_time: Option<Instant>,
    id: usize,
}

impl TraceFnGuard {
    pub fn new(name: &'static str, message: Option<&str>) -> Self {
        let enabled = ENABLE_TRACE.load(Ordering::Relaxed);
        let start_time = if enabled { Some(Instant::now()) } else { None };
        let id = CALL_ID_COUNTER.fetch_add(1, Ordering::Relaxed);

        if enabled {
            with_indent(|i| format!(
                "{}[#{}]↳ Enter: {} {}",
                "  ".repeat(i),
                id,
                name,
                message.unwrap_or("")
            ));
            indent_inc();
        }

        Self {
            name,
            enabled,
            start_time,
            id,
        }
    }
}

impl Drop for TraceFnGuard {
    fn drop(&mut self) {
        if self.enabled {
            indent_dec();
            if let Some(start) = self.start_time {
                let duration = start.elapsed();
                with_indent(|i| format!(
                    "{}[#{}]↳ Exit:  {} (took {:.2?})",
                    "  ".repeat(i),
                    self.id,
                    self.name,
                    duration
                ));
            } else {
                with_indent(|i| format!(
                    "{}↳ Exit:  {}",
                    "  ".repeat(i),
                    self.name
                ));
            }
        }
    }
}

/// Macro to insert tracing into functions.
#[macro_export]
macro_rules! trace_fn {
    ($name:expr) => {
        let _trace_guard = $crate::TraceFnGuard::new($name, None);
    };
    ($name:expr, $($tt:tt)*) => {
        let _trace_guard = $crate::TraceFnGuard::new($name, Some(&format!($($tt)*)));
    };
}

#[macro_export]
macro_rules! trace_log {
    ($($tt:tt)*) => {
            if $crate::ENABLE_TRACE.load(std::sync::atomic::Ordering::Relaxed) {
                $crate::with_indent(|i| format!(
                    "{}      {}",
                    "  ".repeat(i),
                    format!($($tt)*),
                ));
            }
    };
}
