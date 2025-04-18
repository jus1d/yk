use crate::parser::Expr;
use crate::parser::Program;
use crate::parser::Statement;

#[allow(unreachable_patterns)]
pub fn compile_aarch64(program: &Program) {
    for function in &program.functions {
        if &function.name != "main" {
            todo!("compiling only main function is implemented");
        }

        let mut strings = Vec::<String>::new();

        write_preamble();

        println!();

        println!("_main:");
        for statement in &function.body {
            match statement {
                Statement::Ret { value } => {
                    if let Some(value) = value {
                        match value {
                            Expr::Number(number) => {
                                write_epilogue(*number as u8);
                            }
                            _ => todo!("returning only integer values is implemented")
                        }
                    }
                }
                Statement::Funcall { name, args } => {
                    if name.as_str() != "println" {
                        todo!();
                    }

                    let i = strings.len();
                    let text = match &args[0] {
                        Expr::String(s) => s.clone(),
                        _ => String::from("")
                    };
                    strings.push(text.clone());

                    println!("    ; println(\"{}\")", text);
                    println!("    mov     x0, #1");
                    println!("    adrp    x1, strings.{}@PAGE", i);
                    println!("    add     x1, x1, strings.{}@PAGEOFF", i);
                    println!("    mov     x2, #{}", text.len() + 1); // +1 for \n
                    println!("    mov     x16, #4");
                    println!("    svc     #0x80");
                }
                _ => todo!()
            }
        }

        write_data(strings);
    }
}

fn write_preamble() {
    println!(".global _main");
    println!(".align 2");
}

fn write_epilogue(status: u8) {
    println!("    ; exit({})", status);
    println!("    mov     x0, #{}", status);
    println!("    mov     x16, #1");
    println!("    svc     #0x80\n");
}

fn write_data(strings: Vec<String>) {
    for (i, s) in strings.iter().enumerate() {
        println!("strings.{}:", i);
        println!("    .ascii \"{}\\n\"", s);
    }
}
