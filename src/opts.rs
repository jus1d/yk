use std::{env::Args, process::exit};

macro_rules! usage {
    ($program:expr) => {{
        println!("Usage: {} [OPTIONS] <filename>", $program);
        println!("  OPTIONS:");
        println!("    -o <output>           Specify output path");
        println!("    -I<path>              Add `path` to list of include directories");
        println!("    --opt                 Enable compiler optimizations");
        println!("    --unsafe          (!) COMPLETELY disable analyzing and typechecking");
        println!("    --silent, -s          Do not print any logs of compilation");
        println!("    --emit-comments       Emit comments to assembly");
        println!("    --help, -h            Print this help message");
    }};

    () => {
        crate::usage!("ykc")
    }
}

pub struct Opts {
    program_name: String,
    pub input_path: String,
    pub output_path: String,
    pub disable_analyzing: bool,
    pub silent: bool,
    pub emit_comments: bool,
    pub enable_optimization: bool,
    pub include_folders: Vec<String>,
    pub linker_flags: Vec<String>,
}

impl Opts {
    pub fn parse_args(mut args: Args) -> Opts {
        let mut opts = Opts {
            program_name: String::new(),
            input_path: String::new(),
            output_path: String::new(),
            disable_analyzing: false,
            silent: false,
            emit_comments: false,
            enable_optimization: false,
            include_folders: Vec::from(&[String::from("."), String::from("std")]),
            linker_flags: vec![],
        };
        opts.program_name = args.next().unwrap();

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-o" => {
                    match args.next() {
                        Some(output_path) => {
                            opts.output_path = output_path;
                        },
                        None => {
                            usage!(opts.program_name);
                            eprintln!("argument to '-o' is missing");
                            exit(1);
                        },
                    }
                },
                "--emit-comments" => {
                    opts.emit_comments = true;
                },
                "--unsafe" => {
                    opts.disable_analyzing = true;
                },
                "--opt" => {
                    opts.enable_optimization = true;
                },
                "-s" | "--silent" => {
                    opts.silent = true;
                },
                "-h" | "--help" => {
                    usage!(opts.program_name);
                    exit(0);
                },
                _ => {
                    if let Some(linker_flag) = arg.strip_prefix("-L") {
                        opts.linker_flags.push(linker_flag.into());
                    } else if arg.starts_with("-l") || arg.starts_with("-L") {
                        opts.linker_flags.push(arg);
                    } else {
                        // NOTE: If none of conditions: consider arg as input file path
                        opts.input_path = arg;
                    }
                }
            }
        }

        if opts.input_path.is_empty() {
            usage!(opts.program_name);
            eprintln!("no input files");
            exit(1);
        }

        opts
    }
}
