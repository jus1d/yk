pub mod aarch64;

use crate::diag;
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

    pub fn compile<W: Write>(&self, out: &mut W, emit_comments: bool) -> io::Result<()> {
        let mut g = if cfg![target_arch = "aarch64"] {
            aarch64::Generator::new(&self.ast, out, emit_comments)
        } else {
            diag::fatal!("unsupported architecture. only `aarch64` available now");
        };
        g.generate()?;
        Ok(())
    }
}
