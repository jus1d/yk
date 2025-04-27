use std::{env::Args, process::exit};

use crate::diag;

macro_rules! usage {
    ($program:expr) => {{
        println!("Usage: {} [OPTIONS] <filename>", $program);
        println!("  OPTIONS:");
        println!("    -o <output>           Specify output path");
        println!("    --help, -h            Print this help message");
    }};

    () => {
        crate::usage!("ykc")
    }
}

pub struct Opts {
    program_name: String,
    pub input_path: String,
    pub use_custom_output: bool,
    pub output_path: String,
    pub disable_analyzing: bool,
    pub emit_comments: bool,
    pub enable_optimization: bool,
}

impl Opts {
    pub fn parse_args(mut args: Args) -> Opts {
        let mut opts = Opts {
            program_name: String::new(),
            input_path: String::new(),
            use_custom_output: false,
            output_path: String::new(),
            disable_analyzing: false,
            emit_comments: false,
            enable_optimization: false,
        };
        opts.program_name = args.next().unwrap();

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-o" => {
                    match args.next() {
                        Some(output_path) => {
                            opts.use_custom_output = true;
                            opts.output_path = output_path;
                        },
                        None => {
                            usage!(opts.program_name);
                            diag::fatal!("argument to '-o' is missing");
                        },
                    }
                },
                "--emit-comments" => {
                    opts.emit_comments = true;
                },
                "--unsafe" => {
                    opts.disable_analyzing = true;
                },
                "--opt-level" => {
                    opts.enable_optimization = true;
                },
                "-h" | "--help" => {
                    usage!(opts.program_name);
                    exit(0);
                },
                _ => {
                    opts.input_path = arg;
                }
            }
        }

        if opts.input_path.is_empty() {
            usage!(opts.program_name);
            diag::fatal!("no input files");
        }

        if opts.output_path.is_empty() {
            opts.output_path = String::from("stdout");
        }

        opts
    }
}
