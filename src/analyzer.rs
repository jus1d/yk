use crate::{diag, parser};
use std::collections::HashMap;

pub fn check_collisions_with_builtin(program: &parser::Program) {
    let builtins = HashMap::from([
        ("println", parser::Function {
            name: String::from("println"),
            ret_type: String::from("void"),
            params: vec![parser::Param {
                name: String::from("message"),
                typ: String::from("string"),
            }],
            body: vec![],
        }),
        ("exit", parser::Function {
            name: String::from("exit"),
            ret_type: String::from("never"),
            params: vec![parser::Param {
                name: String::from("code"),
                typ: String::from("int64"),
            }],
            body: vec![],
        }),
    ]);

    for function in &program.functions {
        let name = function.name.as_str();
        if builtins.contains_key(name) {
            diag::fatal!("name '{name}' is a builtin function name");
        }

        for statement in &function.body {
            match statement {
                parser::Statement::Funcall { name, args } => {
                    if builtins.contains_key(name.as_str()) {
                        if args.len() != builtins[name.as_str()].params.len() {
                            diag::fatal!("function '{name}' expects {} arguments but got {}", builtins[name.as_str()].params.len(), args.len());
                        }

                        // TODO: type check function arguments
                    } else {
                        diag::fatal!("function '{name}' is not defined");
                    }
                }
                _ => {}
            }
        }
    }
}
