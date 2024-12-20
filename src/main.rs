mod compile;
mod consts;
mod fast_lex;
mod lex;
mod optimize;
mod parse;
mod semantics;
mod slow_lex;
mod slow_parse;
mod stack;
mod structs;
mod typecheck;
mod typecheck_fast;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use compile::compile_prog;
use optimize::optimize_prog;
use parse::parse_prog;

fn main() -> std::io::Result<()> {
    // parse args
    let args: Vec<String> = env::args().collect();
    let in_name = &args[1];
    let out_name = &args[2];

    // load input file
    let mut start = std::time::Instant::now();
    let mut buf = Vec::new();
    File::open(in_name)?.read_to_end(&mut buf).unwrap();
    println!("read in {}", start.elapsed().as_millis());

    // lex
    start = std::time::Instant::now();
    let mut bet_fast_lexed = fast_lex::Lexer::default().lex_fast(&buf);
    println!("lexed in {}", start.elapsed().as_millis());

    // parse
    start = std::time::Instant::now();
    let bet_prog = parse_prog(&mut bet_fast_lexed);
    println!("parsed in {}", start.elapsed().as_millis());

    // type check
    let bet_typed = bet_prog.typecheck();
    println!("typed in {}", start.elapsed().as_millis());

    // compare the fast type-checked program to the old version
    let slow_prog = slow_parse::parse_prog(
        &mut slow_lex::Lexer::default().lex(&String::from_utf8(buf.clone()).unwrap()),
    );
    let slow_typed = slow_prog.typecheck();
    assert_eq!(bet_typed.clone().to_slow(), slow_typed);

    // optimize
    let bet_optimized = optimize_prog(&bet_typed.to_slow());
    println!("optimized in {}", start.elapsed().as_millis());

    // compile
    let bet_compiled = compile_prog(&bet_optimized);
    println!("compiled");

    // write result
    let mut out_file = File::create(out_name)?;
    out_file.write_all(bet_compiled.as_bytes())?;

    Ok(())
}
