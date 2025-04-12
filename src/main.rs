use std::fs;

pub mod lexer;
pub mod parser;

fn main() {
    let filename = "main.w";
    let source = fs::read_to_string(filename).expect("Failed to read file");

    let lexer = lexer::Lexer::new(source.chars(), filename.to_string());
    let mut parser = parser::Parser::from_iter(lexer);

    let program = parser.parse_program();
    println!("{:?}", program);
}
