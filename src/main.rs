use std::io;
use std::fs;
use std::process;

pub mod lexer;
pub mod parser;
pub mod compiler;

fn main() {
    let mut args = std::env::args();
    let program = args.next().unwrap();
    let filename = args.next().unwrap_or_else(|| {
        eprintln!("Usage: {} <filename>", program);
        eprintln!("error: no file path provided");
        process::exit(1);
    });

    let source = fs::read_to_string(filename.clone()).expect("Failed to read file");

    let lexer = lexer::Lexer::new(source.chars(), filename.to_string());
    let mut parser = parser::Parser::from_iter(lexer);

    let program = parser.parse_program();

    if cfg![target_arch = "aarch64"] {
        compiler::generate_asm_aarch64(io::stdout(), &program).unwrap();
    } else {
        eprintln!("error: unsupported architecture. only aarch64 is supported");
        process::exit(1);
    }
}
