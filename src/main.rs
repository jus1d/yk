pub mod compiler;
pub mod analyzer;
pub mod parser;
pub mod lexer;
pub mod diag;
pub mod opts;

use std::io;
use std::fs;

use opts::Opts;

fn main() {
    let args = std::env::args();
    let opts = Opts::parse_args(args);

    let source = fs::read_to_string(&opts.input_file_path).unwrap_or_else(|_| {
        diag::fatal!("failed to read frome file '{}'", opts.input_file_path);
    });

    let lexer = lexer::Lexer::new(source.chars(), &opts.input_file_path);
    let mut parser = parser::Parser::from_iter(lexer);

    let program = parser.parse_program();
    analyzer::analyze(&program);

    if cfg![target_arch = "aarch64"] {
        let mut stdout = io::stdout().lock();
        let compiler = compiler::Compiler::new(program);
        compiler.generate_asm_aarch64_darwin(&mut stdout).unwrap();
    } else {
        diag::fatal!("unsupported architecture. only `aarch64` is supported now");
    }
}
