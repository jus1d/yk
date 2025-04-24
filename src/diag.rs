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
        eprintln!("error: {}", format!($msg $(, $arg)*));
    }};

    ($loc:expr, $($arg:tt)*) => {{
        eprintln!("{}: error: {}", $loc, format!($($arg)*));
    }};
}

#[macro_export]
macro_rules! warning {
    ($msg:literal $(, $arg:expr)* $(,)?) => {{
        eprintln!("warn: {}", format!($msg $(, $arg)*));
    }};

    ($loc:expr, $($arg:tt)*) => {{
        eprintln!("{}: warn:{}", $loc, format!($($arg)*));
    }};
}

pub use {warning, error, fatal};
