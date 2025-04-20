#[macro_export]
macro_rules! colors {
    () => {
        const RED: &str = "\x1b[31m";
        const BOLD: &str = "\x1b[1m";
        const RESET: &str = "\x1b[0m";
    };
}

#[macro_export]
macro_rules! fatal {
    ($($arg:tt)*) => {{
        $crate::error!($($arg)*);
        std::process::exit(1);
    }};
}

#[macro_export]
macro_rules! error {
    ($msg:literal $(, $arg:expr)* $(,)?) => {{
        $crate::colors!();
        eprintln!("{BOLD}{RED}error:{RESET}{BOLD} {}{RESET}", format!($msg $(, $arg)*))
    }};

    ($loc:expr, $($arg:tt)*) => {{
        $crate::colors!();
        eprintln!("{BOLD}{}: {RED}error:{RESET}{BOLD} {}{RESET}", $loc, format!($($arg)*))
    }};
}

// Re-export macros at module level
pub use error;
pub use fatal;
