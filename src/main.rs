pub mod optimizer;
pub mod compiler;
pub mod analyzer;
pub mod parser;
pub mod lexer;
pub mod diag;
pub mod opts;

use std::{fs, process::exit};
use opts::Opts;

fn main() {
    let args = std::env::args();
    let opts = Opts::parse_args(args);

    if opts.disable_analyzing && opts.enable_optimization {
        diag::fatal!("cannot enable optimizations, if `--unsafe` used")
    }

    let source = fs::read_to_string(&opts.input_path).unwrap_or_else(|_| {
        eprintln!("error: failed to read from file '{file}'", file = opts.input_path);
        exit(1);
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
