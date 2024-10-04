mod consts;
mod parse;
mod semantics;
mod structs;
mod typecheck;
mod compile;

use crate::compile::compile_prog;

use core::panic;
use std::fs::File;
use std::io::prelude::*;
use std::env;

use sexp::*;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let parsed = &parse(&format!("({})", in_contents).to_string());
    let prog = match parsed {
        Ok(sexp) => parse::parse_program(sexp),
        Err(err) => panic!("Invalid: Parsing failed with error {:?}", err),
    };

    let result = compile_prog(&prog);

    // println!("Expected: {}", compute(&expr));

    let asm_program = format!(
        "
section .text

extern snek_print
extern snek_error

global our_code_starts_here

{}
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
