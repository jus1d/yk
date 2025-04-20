#[macro_export]
macro_rules! colors {
    () => {{
        const YELLOW: &str = "\x1b[33m";
        const RED: &str = "\x1b[31m";
        const BOLD: &str = "\x1b[1m";
        const RESET: &str = "\x1b[0m";
        (YELLOW, RED, BOLD, RESET)
    }};
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
        let (_, red, bold, reset) = $crate::colors!();
        eprintln!("{}{}error:{}{} {}{}", bold, red, reset, bold, format!($msg $(, $arg)*), reset);
    }};

    ($loc:expr, $($arg:tt)*) => {{
        let (_, red, bold, reset) = $crate::colors!();
        eprintln!("{}{}: {}error:{}{} {}{}", bold, $loc, red, reset, bold, format!($($arg)*), reset);
    }};
}

#[macro_export]
macro_rules! warning {
    ($msg:literal $(, $arg:expr)* $(,)?) => {{
        let (yellow, _, bold, reset) = $crate::colors!();
        eprintln!("{}{}warn:{}{} {}{}", bold, yellow, reset, bold, format!($msg $(, $arg)*), reset);
    }};

    ($loc:expr, $($arg:tt)*) => {{
        let (yellow, _, bold, reset) = $crate::colors!();
        eprintln!("{}{}: {}warn:{}{} {}{}", bold, $loc, yellow, reset, bold, format!($($arg)*), reset);
    }};
}

pub use {error, fatal, warning};
