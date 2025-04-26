use std::collections::HashMap;

use crate::parser::{Ast, Statement, Function, Param, Expr};
use crate::diag;

pub fn analyze(ast: &Ast) {
    check_entrypoint_declaration(ast);
    check_funcs_and_calls(ast);
}

fn check_funcs_and_calls(ast: &Ast) {
    let builtin_funcs = HashMap::from([
        ("puts", Function {
            name: String::from("puts"),
            ret_type: String::from("void"),
            params: vec![Param {
                name: String::from("str"),
                typ: String::from("string"),
            }],
            body: vec![],
        }),
        ("exit", Function {
            name: String::from("exit"),
            ret_type: String::from("never"),
            params: vec![Param {
                name: String::from("code"),
                typ: String::from("int64"),
            }],
            body: vec![],
        }),
    ]);

    for (name, _) in &ast.functions {
        if builtin_funcs.contains_key(name.as_str()) {
            diag::fatal!("symbol '{name}' is a builtin function name");
        }
    }

    for function in ast.functions.values() {
        for statement in &function.body {
            match statement {
                Statement::Funcall { name, args } => {
                    validate_funcall(name, args, &builtin_funcs, &ast.functions);
                },
                Statement::Ret { value: _ } => {
                    // TODO: check return type matches function's declared return type
                }
            }
        }
    }
}

fn validate_funcall(name: &str, args: &[Expr], builtin_funcs: &HashMap<&str, Function>, user_funcs: &HashMap<String, Function>) {
    // TODO: type check function arguments against expected params
    if builtin_funcs.contains_key(name) {
        let expected_params = &builtin_funcs[name].params;
        check_arguments_count(name, args.len(), expected_params.len());
    } else if let Some(user_func) = user_funcs.get(name) {
        check_arguments_count(name, args.len(), user_func.params.len());
    } else {
        diag::fatal!("call to undeclared function '{name}'");
    }
}

fn check_arguments_count(func_name: &str, actual: usize, expected: usize) {
    match actual.cmp(&expected) {
        std::cmp::Ordering::Greater => {
            diag::fatal!("too many arguments to function call '{func_name}', expected {expected} arguments, have {actual}");
        }
        std::cmp::Ordering::Less => {
            diag::fatal!("too few arguments to function call '{func_name}', expected {expected} arguments, have {actual}");
        }
        std::cmp::Ordering::Equal => {}
    }
}

fn check_entrypoint_declaration(ast: &Ast) {
    if !ast.functions.contains_key("main") {
        diag::fatal!("entry point not declared. expected: 'fn main() int64'");
    }

    let func = ast.functions.get("main").unwrap();
    if func.params.len() > 0 {
        diag::fatal!("function 'main' should not have parameters");
    }
    if func.ret_type.as_str() != "int64" {
        diag::fatal!("unexpected main function declaration, expected 'fn main() int64'");
    }
}
