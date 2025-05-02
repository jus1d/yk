pub mod optimizer;
pub mod compiler;
pub mod analyzer;
pub mod parser;
pub mod lexer;
pub mod diag;
pub mod opts;

use std::path::Path;
use std::fs::File;
use std::fs;
use std::process::Command;
use opts::Opts;
use parser::Ast;

fn main() {
    let args = std::env::args();
    let opts = Opts::parse_args(args);

    let path = Path::new(&opts.input_path).with_extension("");
    let filebase = path.as_os_str().to_str().unwrap();

    let source = fs::read_to_string(&opts.input_path).unwrap_or_else(|_| {
        diag::fatal!("failed to read from file '{}'", opts.input_path);
    });

    let lexer = lexer::Lexer::new(source.chars(), &opts.input_path);
    let mut parser = parser::Parser::from_iter(lexer);

    let mut ast = parser.parse_ast();
    if !opts.disable_analyzing {
        analyzer::analyze(&ast);
    }

    if opts.enable_optimization {
        optimizer::precompute_expressions(&mut ast);
        optimizer::eliminate_deadcode(&mut ast);
    }

    // main.s generated
    let assembly_path = format!("{}.s", filebase);
    generate_assembly_from_ast(ast, &opts, &assembly_path);

    let object_path = format!("{}.o", filebase);
    generate_object_from_assembly(&assembly_path, &object_path);

    let output_path = if opts.output_path.is_empty() {
        format!("{}", filebase)
    } else {
        opts.output_path
    };
    link_object_file(&object_path, &output_path);

    // let compiler = compiler::Compiler::new(ast);
    // if opts.output_path.is_empty() {
    //     let mut stdout = io::stdout().lock();
    //     compiler.compile(&mut stdout, opts.emit_comments).unwrap();
    // } else {
    //     let mut file = match File::create(&opts.output_path) {
    //         Ok(file) => file,
    //         Err(err) => diag::fatal!("failed to open file `{}`: {}", opts.output_path, err),
    //     };
    //     compiler.compile(&mut file, opts.emit_comments).unwrap();
    // };
}

fn generate_assembly_from_ast(ast: Ast, opts: &Opts, path: &str) {
    let compiler = compiler::Compiler::new(ast);

    let mut file = match File::create(path) {
        Ok(file) => file,
        Err(err) => diag::fatal!("failed to open file `{}`: {}", path, err),
    };
    compiler.compile(&mut file, opts.emit_comments).unwrap();
}

fn generate_object_from_assembly(assembly_path: &str, object_path: &str) {
    Command::new("as")
        .args(&["-arch", "arm64", "-o", object_path, assembly_path])
        .output().unwrap_or_else(|_| {
            diag::fatal!("cannot create object file");
        });
}

fn link_object_file(object_path: &str, output_path: &str) {
    let sdk_output = Command::new("xcrun")
        .args(["--show-sdk-path"])
        .output().unwrap().stdout;
    let sdk_path = String::from_utf8_lossy(&sdk_output).trim().to_string();

    Command::new("ld")
        .args([
            "-o", output_path,
            object_path,
            "-lSystem",
            "-syslibroot",
            &sdk_path,
            "-e", "_main",
            "-arch", "arm64"
        ])
        .status().unwrap();
}
