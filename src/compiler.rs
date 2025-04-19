use crate::parser::Expr;
use crate::parser::Program;
use crate::parser::Statement;

use std::io;
use std::io::Write;

#[allow(unreachable_patterns)]
pub fn generate_asm_aarch64(mut out: impl Write, program: &Program) -> io::Result<()> {
    for function in &program.functions {
        if &function.name != "main" {
            todo!("compiling only main function is implemented");
        }

        let mut strings = Vec::<String>::new();

        generate_preamble(&mut out)?;

        writeln!(out, "_main:")?;
        for statement in &function.body {
            match statement {
                Statement::Ret { value } => {
                    if let Some(value) = value {
                        match value {
                            Expr::Number(number) => {
                                generate_epilogue(&mut out, *number as u8)?;
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
                        Expr::String(s) => s.as_str(),
                        _ => ""
                    };
                    strings.push(text.to_string());

                    writeln!(out, "    ; println(\"{}\")", text)?;
                    writeln!(out, "    mov     x0, #1")?;
                    writeln!(out, "    adrp    x1, strings.{}@PAGE", i)?;
                    writeln!(out, "    add     x1, x1, strings.{}@PAGEOFF", i)?;
                    writeln!(out, "    mov     x2, #{}", text.len() + 1)?; // +1 for \n
                    writeln!(out, "    mov     x16, #4")?;
                    writeln!(out, "    svc     #0x80")?;
                }
                _ => todo!()
            }
        }

        generate_data(&mut out, strings)?;

    }
    Ok(())
}

fn generate_preamble(mut out: impl Write) -> io::Result<()> {
    writeln!(out, ".global _main")?;
    writeln!(out, ".align 2")?;
    Ok(())
}

fn generate_epilogue(mut out: impl Write, status: u8) -> io::Result<()> {
    writeln!(out, "    ; exit({})", status)?;
    writeln!(out, "    mov     x0, #{}", status)?;
    writeln!(out, "    mov     x16, #1")?;
    writeln!(out, "    svc     #0x80")?;
    Ok(())
}

fn generate_data(mut out: impl Write, strings: Vec<String>) -> io::Result<()> {
    for (i, s) in strings.iter().enumerate() {
        writeln!(out, "strings.{}:", i)?;
        writeln!(out, "    .ascii \"{}\\n\"", s)?;
    }
    Ok(())
}
