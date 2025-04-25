pub mod compiler;
pub mod analyzer;
pub mod parser;
pub mod lexer;
pub mod diag;
pub mod opts;

use std::fs::File;
use std::io;
use std::fs;

use opts::Opts;

fn main() {
    let args = std::env::args();
    let opts = Opts::parse_args(args);

    let source = fs::read_to_string(&opts.input_path).unwrap_or_else(|_| {
        diag::fatal!("failed to read frome file '{}'", opts.input_path);
    });

    let lexer = lexer::Lexer::new(source.chars(), &opts.input_path);
    let mut parser = parser::Parser::from_iter(lexer);

    let ast = parser.parse_ast();
    analyzer::analyze(&ast);

    if cfg![target_arch = "aarch64"] {
        let compiler = compiler::Compiler::new(ast);
        if opts.use_custom_output {
            let mut file = match File::create(&opts.output_path) {
                Ok(file) => file,
                Err(err) => diag::fatal!("failed to open file `{}`: {}", opts.output_path, err),
            };
            compiler.compile(&mut file).unwrap();
        } else {
            let mut stdout = io::stdout().lock();
            compiler.compile(&mut stdout).unwrap();
        };
    } else {
        diag::fatal!("unsupported architecture. only `aarch64` is supported now");
    }
}
