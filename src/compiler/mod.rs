pub mod aarch64;

use crate::opts::Opts;
use crate::parser::ast;

use ast::Ast;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::exit;
use std::time::Instant;

pub struct Compiler {
    ast: Ast,
}

impl Compiler {
    pub fn new(ast: Ast) -> Self {
        Compiler {
            ast,
        }
    }

    pub fn compile(&self, opts: &Opts) {
        if cfg![target_arch = "aarch64"] {
            compile_aarch64_darwin(&self.ast, opts);
        } else {
            eprintln!("unsupported architecture. only `aarch64` available now");
            exit(1);
        };
    }

}

fn compile_aarch64_darwin(ast: &Ast, opts: &Opts) {
    let filebase = if opts.output_path.is_empty() {
        let path = Path::new(&opts.input_path).with_extension("");
        path.as_os_str().to_str().unwrap().to_string()
    } else {
        let path = Path::new(&opts.output_path).with_extension("");
        path.as_os_str().to_str().unwrap().to_string()
    };
    let extension = Path::new(&opts.output_path).extension().and_then(|ext| ext.to_str()).unwrap_or("");
    let assembly_path = format!("{}.s", filebase);

    let start = Instant::now();
    let mut g = aarch64::Generator::new(ast, opts.emit_comments);
    g.generate().unwrap_or_else(|err| {
        eprintln!("error: cannot generate assembly: {}", err);
        exit(1);
    });

    let mut out = match File::create(&assembly_path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("failed to open file `{}`: {}", &assembly_path, err);
            exit(1);
        },
    };

    let bytes = g.into_bytes();
    match out.write_all(&bytes) {
        Err(err) => {
            eprintln!("cannot write assembly: {}", err);
            exit(1);
        },
        Ok(_) => {
            let elapsed = start.elapsed();
            if !opts.silent {
                println!("INFO: Generated {} bytes of assembly, took {:.2?}", bytes.len(), elapsed);
            }
        },
    }

    if extension == "s" {
        return;
    }

    let start = Instant::now();
    let object_path = format!("{}.o", filebase);
    aarch64::generate_object_from_assembly(!opts.silent, &assembly_path, &object_path);

    if extension == "o" {
        return;
    }

    aarch64::link_object_file(!opts.silent, &object_path, &filebase);
    let elapsed = start.elapsed();
    if !opts.silent {
        println!("INFO: Generating object and linking took {:.2?}", elapsed);
    }
}
