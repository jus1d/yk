pub mod aarch64;

use crate::parser::Ast;

use std::io;
use std::io::Write;

pub struct Compiler {
    ast: Ast,
}

impl Compiler {
    pub fn new(ast: Ast) -> Self {
        Compiler {
            ast: ast.clone(),
        }
    }

    pub fn compile<W: Write>(&self, out: &mut W) -> io::Result<()> {
        aarch64::generate_aarch64_darwin_assembly(&self.ast, out)?;
        Ok(())
    }
}

pub fn get_variable_position(ast: &Ast, func_name: &str, variable_name: &str) -> usize {
    if let Some(func) = ast.functions.get(func_name) {
        if let Some(_) = func.params.iter().find(|param| param.name == variable_name) {
            let position = func.params.iter().position(|p| p.name == variable_name).unwrap();
            return position;
        } else {
            todo!();
        }
    } else {
        todo!();
    }
}
