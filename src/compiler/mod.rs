pub mod aarch64;

use crate::diag;
use crate::opts::Opts;
use crate::parser::ast;

use ast::Ast;
use std::fs::File;
use std::io::Write;
use std::path::Path;
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
            compile_aarch64_darwin(opts, &self.ast);
        } else {
            diag::fatal!("unsupported architecture. only `aarch64` available now");
        };
    }

}

fn compile_aarch64_darwin(opts: &Opts, ast: &Ast) {
    let filebase = if opts.output_path.is_empty() {
        let path = Path::new(&opts.input_path).with_extension("");
        path.as_os_str().to_str().unwrap().to_string()
    } else {
        opts.output_path.clone()
    };

    let assembly_path = format!("{}.s", filebase);

    let start = Instant::now();
    let mut g = aarch64::Generator::new(ast, opts.emit_comments);
    g.generate().unwrap_or_else(|err| {
        diag::fatal!("cannot generate assembly: {}", err)
    });

    let mut out = match File::create(&assembly_path) {
        Ok(file) => file,
        Err(err) => diag::fatal!("failed to open file `{}`: {}", &assembly_path, err),
    };

    let bytes = g.into_bytes();
    match out.write_all(&bytes) {
        Err(err) => diag::fatal!("cannot write assembly: {}", err),
        Ok(_) => {
            let elapsed = start.elapsed();
            if !opts.silent {
                println!("INFO: Wrote {} bytes of assembly", bytes.len());
                println!("INFO: Assembly generation took {:.2?}", elapsed);
            }
        },
    }

    let start = Instant::now();
    let object_path = format!("{}.o", filebase);
    aarch64::generate_object_from_assembly(!opts.silent, &assembly_path, &object_path);

    aarch64::link_object_file(!opts.silent, &object_path, &filebase);
    let elapsed = start.elapsed();
    if !opts.silent {
        println!("INFO: Generating object and linking took {:.2?}", elapsed);
    }
}
