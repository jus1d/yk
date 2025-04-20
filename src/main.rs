pub mod lexer;
pub mod parser;
pub mod compiler;
pub mod diag;

use std::io;
use std::fs;

fn main() {
    let mut args = std::env::args();
    let program = args.next().unwrap();
    let filename = args.next().unwrap_or_else(|| {
        eprintln!("Usage: {} <filename>", program);
        diag::fatal!("no file path provided");
    });

    let source = fs::read_to_string(filename.clone()).expect("Failed to read file");

    let lexer = lexer::Lexer::new(source.chars(), filename.to_string());
    let mut parser = parser::Parser::from_iter(lexer);

    let program = parser.parse_program();

    if cfg![target_arch = "aarch64"] {
        compiler::generate_asm_aarch64(io::stdout(), &program).unwrap();
    } else {
        diag::fatal!("unsupported architecture. only `aarch64` is supported now");
    }
}
