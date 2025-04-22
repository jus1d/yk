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

    for (name, function) in &program.functions {
        if builtins.contains_key(name.as_str()) {
            diag::fatal!("symbol '{name}' is a builtin function name");
        }

        for statement in &function.body {
            match statement {
                parser::Statement::Funcall { name, args } => {
                    if builtins.contains_key(name.as_str()) {
                        if args.len() > builtins[name.as_str()].params.len() {
                            diag::fatal!("too many arguments to function call '{name}', expected {} arguments, have {}", builtins[name.as_str()].params.len(), args.len());
                        } else if args.len() < builtins[name.as_str()].params.len() {
                            diag::fatal!("too few arguments to function call '{name}', expected {} arguments, have {}", builtins[name.as_str()].params.len(), args.len());
                        }

                        // TODO: type check function arguments
                    } else {
                        diag::fatal!("call to undeclared function '{name}'");
                    }
                }
                _ => {}
            }
        }
    }
}
