mod compile;
mod consts;
mod lex;
mod parse;
mod parse_bet;
mod semantics;
mod structs;
mod transpile;
mod typecheck;

use crate::compile::compile_prog;

use core::panic;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use parse_bet::parse_bet_program;
use sexp::*;

fn transform_path(original_path: &str) -> std::path::PathBuf {
    let path = std::path::Path::new(original_path);

    // Get the file name without extension
    let file_stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");

    // Get parent directory
    let parent = path
        .parent()
        .and_then(|p| p.parent()) // Get grandparent since we want to replace the last dir
        .unwrap_or(std::path::Path::new(""));

    // Create new path with modified directory and extension
    parent
        .join(format!(
            "{}-bet",
            path.parent()
                .unwrap()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
        ))
        .join(format!("{}.bet", file_stem))
}

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

    let asm_program = compile_prog(&prog);

    // construct the bet program from the AST
    let bet_prog_str = transpile::prog_to_bet(&prog);
    let bet_lexed = lex::lex(&bet_prog_str, lex::LexerConfig::default());
    let bet_prog = parse_bet_program(&bet_lexed);
    let bet_compiled = compile_prog(&bet_prog); // make sure it still compiles
    assert!(bet_compiled == asm_program);

    // we have a program which compiled for snek; make sure it still compiles
    let new_in_path = transform_path(&in_name);
    let _ = File::create(&new_in_path).and_then(|mut file| file.write_all(bet_prog_str.as_bytes()));

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
