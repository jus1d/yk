use std::collections::HashMap;

use crate::parser::{Program, Statement, Function, Param};
use crate::diag;

pub fn analyze(program: &Program) {
    check_entrypoint_declaration(program);
    check_collisions_with_builtin(program);
}

fn check_collisions_with_builtin(program: &Program) {
    let builtins = HashMap::from([
        ("println", Function {
            name: String::from("println"),
            ret_type: String::from("void"),
            params: vec![Param {
                name: String::from("message"),
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

    for (name, function) in &program.functions {
        if builtins.contains_key(name.as_str()) {
            diag::fatal!("symbol '{name}' is a builtin function name");
        }

        for statement in &function.body {
            match statement {
                Statement::Funcall { name, args } => {
                    // TODO: factor args checking into a separate function
                    if builtins.contains_key(name.as_str()) {
                        let params_count = builtins[name.as_str()].params.len();
                        if args.len() > params_count {
                            diag::fatal!("too many arguments to function call '{name}', expected {} arguments, have {}", builtins[name.as_str()].params.len(), args.len());
                        } else if args.len() < params_count {
                            diag::fatal!("too few arguments to function call '{name}', expected {} arguments, have {}", builtins[name.as_str()].params.len(), args.len());
                        }

                        // TODO: type check function arguments
                    } else if !program.functions.contains_key(name.as_str()) {
                        let params_count = program.functions[name.as_str()].params.len();
                        if args.len() > params_count {
                            diag::fatal!("too many arguments to function call '{name}', expected {} arguments, have {}", builtins[name.as_str()].params.len(), args.len());
                        } else if args.len() < params_count {
                            diag::fatal!("too few arguments to function call '{name}', expected {} arguments, have {}", builtins[name.as_str()].params.len(), args.len());
                        }

                        // TODO: type check function arguments
                    } else {
                        diag::fatal!("call to undeclared function '{name}'");
                    }
                },
                Statement::Ret { value: _ } => {
                    // TODO: check return type
                }
            }
        }
    }
}

fn check_entrypoint_declaration(program: &Program) {
    if !program.functions.contains_key("main") {
        diag::fatal!("entry point not declared. expected: 'fn main() int64'");
    }

    let func = program.functions.get("main").unwrap();
    if func.params.len() > 0 {
        diag::fatal!("function 'main' should not have parameters");
    }
    if func.ret_type.as_str() != "int64" {
        diag::fatal!("unexpected main function declaration, expected 'fn main() int64'");
    }
}
