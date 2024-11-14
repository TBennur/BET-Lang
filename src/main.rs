mod compile;
mod consts;
mod lex;
mod parse;
mod semantics;
mod structs;
mod typecheck;

use crate::compile::compile_prog;

use core::panic;
use std::env;
use std::fs::File;
use std::io::prelude::*;

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
    let new_in_path = transform_path(&in_name);
    if let Ok(mut new_in_file) = File::open(new_in_path.clone()) {
        let mut new_in_contents = String::new();
        new_in_file.read_to_string(&mut new_in_contents)?;
        let lexed = lex::lex(&new_in_contents, lex::LexerConfig::default());
        println!("{:#?}", lexed);
    };

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let parsed = &parse(&format!("({})", in_contents).to_string());
    let prog = match parsed {
        Ok(sexp) => parse::parse_program(sexp),
        Err(err) => panic!("Invalid: Parsing failed with error {:?}", err),
    };

    let asm_program = compile_prog(&prog);

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
