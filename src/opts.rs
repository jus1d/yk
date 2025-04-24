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
    pub input_file_path: String,
    pub output_file_path: String,
}

impl Opts {
    pub fn parse_args(mut args: Args) -> Opts {
        let mut opts = Opts {
            program_name: String::new(),
            input_file_path: String::new(),
            output_file_path: String::new(),
        };
        opts.program_name = args.next().unwrap();

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-o" => {
                    match args.next() {
                        Some(output_file_path) => {
                            opts.output_file_path = output_file_path;
                        },
                        None => {
                            usage!(opts.program_name);
                            diag::fatal!("argument to '-o' is missing");
                        },
                    }
                },
                "-h" | "--help" => {
                    usage!(opts.program_name);
                    exit(0);
                },
                _ => {
                    opts.input_file_path = arg;
                }
            }
        }

        if opts.input_file_path.is_empty() {
            usage!(opts.program_name);
            diag::fatal!("no input files");
        }

        opts
    }
}
