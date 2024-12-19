mod compile;
mod consts;
mod lex;
mod slow_lex;
mod fast_lex;
mod optimize;
mod parse;
mod semantics;
mod structs;
mod typecheck;
mod stack;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use compile::compile_prog;
use optimize::optimize_prog;
use parse::parse_prog;

fn main() -> std::io::Result<()> {
    let mut start = std::time::Instant::now();
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // load input file
    let mut in_contents = String::new();
    File::open(in_name)?.read_to_string(&mut in_contents)?;
    
    let mut buf = Vec::new();
    File::open(in_name)?.read_to_end(&mut buf).unwrap();
    
    // construct program

    println!("read in {}", start.elapsed().as_millis());
    start = std::time::Instant::now();

    let mut bet_lexed = slow_lex::Lexer::default().lex(&in_contents);
    let mut bet_fast_lexed = fast_lex::Lexer::default().lex_fast(buf);
    // assert_eq!(bet_lexed, bet_fast_lexed);
    println!("lexed in {}", start.elapsed().as_millis());
    start = std::time::Instant::now();

    let bet_prog = parse_prog(&mut bet_lexed);
    println!("parsed in {}", start.elapsed().as_millis());

    let bet_typed = bet_prog.typecheck();
    println!("typed in {}", start.elapsed().as_millis());

    let bet_optimized = optimize_prog(&bet_typed);
    println!("optimized in {}", start.elapsed().as_millis());

    let bet_compiled = compile_prog(&bet_optimized);
    println!("compiled");

    let mut out_file = File::create(out_name)?;
    out_file.write_all(bet_compiled.as_bytes())?;

    Ok(())
}
