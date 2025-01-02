mod alt_parse;
mod alt_stack;
mod alt_stack_variants;
mod compile;
mod compile_fast;
mod compile_shared;
mod consts;
mod fast_lex;
mod lex;
mod optimize;
mod parse;
mod parse_shared;
mod semantics;
mod slow_lex;
mod slow_parse;
mod stack;
mod structs;
mod to_slow;
mod typecheck;
mod typecheck_fast;
mod typecheck_fast_alt;
mod typecheck_fast_shared;
mod drop;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use compile::{compile_prog, StructSerializer};
use compile_shared::emit_program;
use lex::Lexpr;
use optimize::optimize_prog;
use parse_shared::parse_prog;

fn main() -> std::io::Result<()> {
    // parse args
    let args: Vec<String> = env::args().collect();
    let in_name = &args[1];
    let out_name = &args[2];

    // load input file
    let mut start = std::time::Instant::now();
    let mut buf = Vec::new();
    File::open(in_name)?.read_to_end(&mut buf).unwrap();
    let buf = buf; // no longer needs to be mutable
    println!("read in {}", start.elapsed().as_millis());

    let check: bool = true;

    // lex
    start = std::time::Instant::now();
    let mut bet_fast_lexed = fast_lex::Lexer::default().lex_fast(&buf);
    println!("lexed in {}", start.elapsed().as_millis());

    let mut dup_lexted = if check {
        fast_lex::Lexer::default().lex_fast(&buf)
    } else {
        Lexpr::Stolen
    };

    // parse
    start = std::time::Instant::now();
    let bet_prog = parse_prog(&mut bet_fast_lexed, parse_shared::Variant::Old);
    println!("parsed in {}", start.elapsed().as_millis());

    if check {
        start = std::time::Instant::now();
        let bet_prog_alt = parse_prog(&mut dup_lexted, parse_shared::Variant::Alt);
        println!("parsed alt in {}", start.elapsed().as_millis());
    }

    // type check
    start = std::time::Instant::now();
    let bet_typed = bet_prog.typecheck();
    println!("typed in {}", start.elapsed().as_millis());

    if check {
        // compare the fast type-checked program to the old version
        let alt_buf = &String::from_utf8(buf.clone()).unwrap();

        start = std::time::Instant::now();
        let slow_prog = slow_parse::parse_prog(&mut slow_lex::Lexer::default().lex(alt_buf));
        println!("slow parsed in {}", start.elapsed().as_millis());

        start = std::time::Instant::now();
        let slow_typed = slow_prog.typecheck();
        println!("slow typed in {}", start.elapsed().as_millis());

        let as_slow = bet_typed.to_slow();
        assert_eq!(slow_typed, as_slow);

        // compare the fast type-checked alt program to the old version
        let mut lexed = fast_lex::Lexer::default().lex_fast(&buf);
        let bet_prog = parse_prog(&mut lexed, parse_shared::Variant::Alt);
        start = std::time::Instant::now();
        let alt_fast_typed: structs::FastTypedProg<'_> = bet_prog.typecheck_alt();
        print!("alt typed in {}\n", start.elapsed().as_millis());
        let as_slow = alt_fast_typed.to_slow();
        assert_eq!(slow_typed, as_slow);
    }

    // // optimize
    // let bet_optimized = optimize_prog(&bet_typed.to_slow());
    // println!("optimized in {}", start.elapsed().as_millis());
    let mut bet_optimized = bet_typed;

    // compile
    start = std::time::Instant::now();
    let bet_compiled = compile_fast::compile_prog(&mut bet_optimized);
    println!("compiled in {}", start.elapsed().as_millis());

    if check {
        let bet_optimized_slow = bet_optimized.to_slow();
        start = std::time::Instant::now();
        let bet_compiled_slow = compile_prog(&bet_optimized_slow);
        println!("compiled slow in {}", start.elapsed().as_millis());

        assert_eq!(bet_compiled, bet_compiled_slow);
    }

    // serialize
    start = std::time::Instant::now();
    let struct_serializer = bet_optimized.make_clone_struct_serializer();
    if check {
        let slow_struct_serializer = match bet_optimized.to_slow() {
            structs::TypedProg::Program(_, struct_sigs, struct_layouts, _, _) => StructSerializer {
                struct_sigs,
                struct_layouts,
            },
        };
        assert_eq!(struct_serializer, slow_struct_serializer);
    }

    // emit program String from Vec<Instr>
    let estimated_len = bet_compiled.len() * 15;
    let tmp = compile_shared::ProgInstr {
        instrs: bet_compiled,
        struct_serializer,
    };

    // write result to buf
    let mut buf: Vec<u8> = Vec::with_capacity(estimated_len);
    write!(&mut buf, "{}", tmp)?;
    println!("Wrote to buf in {}", start.elapsed().as_millis());

    // write buf to file
    start = std::time::Instant::now();
    let mut out_file = File::create(out_name).unwrap();
    out_file.write_all(&buf)?;
    println!("Wrote to {out_name} in {}", start.elapsed().as_millis());

    Ok(())
}
