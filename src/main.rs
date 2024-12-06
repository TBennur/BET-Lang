mod compile;
mod consts;
mod lex;
mod optimize;
mod parse;
mod semantics;
mod structs;
mod typecheck;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use compile::compile_prog;
use lex::{lex, LexerConfig};
use optimize::optimize_prog;
use parse::parse_prog;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // load input file
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // construct program
    let mut bet_lexed = lex(&in_contents, LexerConfig::default());
    println!("lexed");
    let bet_prog = parse_prog(&mut bet_lexed);
    println!("parsed");
    // println!("{:?}", bet_prog);
    let bet_typed = bet_prog.typecheck();
    println!("typed");
    let bet_optimized = optimize_prog(&bet_typed);
    println!("optimized");
    let bet_compiled = compile_prog(&bet_optimized);
    println!("compiled");

    let mut out_file = File::create(out_name)?;
    out_file.write_all(bet_compiled.as_bytes())?;

    Ok(())
}
