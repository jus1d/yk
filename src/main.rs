use std::fs;
use std::process;

pub mod lexer;
pub mod parser;

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
    println!("{:?}", program);
}
