mod compile;
mod consts;
mod lex;
mod strip;
mod parse;
mod semantics;
mod structs;
mod typecheck;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use lex::{lex, LexerConfig};
use compile::compile_prog;
use parse::parse_program;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // load input file
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    
    // construct program
    let bet_lexed = lex(&in_contents, LexerConfig::default());
    let bet_prog = parse_program(&bet_lexed);
    let bet_compiled = compile_prog(&bet_prog); // make sure it still compiles

    let mut out_file = File::create(out_name)?;
    out_file.write_all(bet_compiled.as_bytes())?;

    Ok(())
}
