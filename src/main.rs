pub mod lexer;
pub mod parser;

fn main() {
    let source = String::from("println(x, \"hui\", 2 * 2);");

    let lexer = lexer::Lexer::new(source.chars());
    let mut parser = parser::Parser::from_iter(lexer);

    let stmt = parser.parse_stmt();
    println!("{:?}", stmt);
}
