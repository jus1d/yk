use crate::parser::Expr;
use crate::parser::Program;
use crate::parser::Statement;

pub fn compile_aarch64(program: &Program) {
    for function in &program.functions {
        if &function.name != "main" {
            todo!("compiling only main function is implemented");
        }

        println!(".global _main");
        println!(".align 2\n");

        println!("_main:");

        for statement in &function.body {
            match statement {
                Statement::Ret { value } => {
                    if let Some(value) = value {
                        match value {
                            Expr::Number(number) => {
                                println!("    mov     x0, #{}", number);
                            }
                            _ => todo!("compiling only integer values is implemented")
                        }
                    }
                }
                _ => todo!("compiling only ret statement is implemented")
            }
        }

        println!("    mov     x16, #1");
        println!("    svc     #0x80");
    }
}
