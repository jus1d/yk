pub mod optimizer;
pub mod compiler;
pub mod analyzer;
pub mod parser;
pub mod lexer;
pub mod diag;
pub mod opts;

use std::fs;
use opts::Opts;

fn main() {
    let args = std::env::args();
    let opts = Opts::parse_args(args);

    let source = fs::read_to_string(&opts.input_path).unwrap_or_else(|_| {
        diag::fatal!("failed to read from file '{}'", opts.input_path);
    });

    let lexer = lexer::Lexer::new(source.chars(), &opts.input_path);
    let mut parser = parser::Parser::from_iter(lexer, opts.include_folders.clone());

    let mut ast = parser.parse_ast();
    if !opts.disable_analyzing {
        analyzer::check_entrypoint_declaration(&ast);
        analyzer::typecheck(&ast);
    }

    if opts.enable_optimization {
        optimizer::precompute_expressions(&mut ast);
        optimizer::eliminate_unused_functions(&mut ast);
    }

    let compiler = compiler::Compiler::new(ast);
    compiler.compile(&opts);
}
