use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
}

#[derive(Debug)]
enum Expr {
    Number(i32),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
}

const WORD_SIZE : i32 = 8;

/// Helper function for converting Sexps into Exprs
/// Mutually recursive with parse_expr, helps parse let bindings
fn parse_binds(binds: &Vec<Sexp>) -> Vec<(String, Expr)> {
    if binds.len() == 0 {
        panic!("Invalid")
    }

    let parsed : Vec<(String, Expr)> = binds.iter().map(|bind| 
        match bind {
            Sexp::List(vec) => {
                match &vec[..] {
                    [Sexp::Atom(S(name)), e] => (name.to_string(), parse_expr(e)),
                    _ => panic!("Invalid"),
                }
            },
            _ => panic!("Invalid"),
        }
    ).collect();
    return parsed;
}

/// Converts Sexps into Exprs
/// Mutually recursive with parse_binds, parses everything but let Exprs
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i32::try_from(*n).unwrap()),
        Sexp::Atom(S(t)) => Expr::Id(t.to_string()),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), Sexp::List(binds), e] if op == "let" => Expr::Let(parse_binds(binds), Box::new(parse_expr(e))),
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

/// Increments each index in a binding dictionary by 1
/// Helps maintain alignment when new values are added
fn incr_binds(binds: HashMap::<String, i32>) -> HashMap::<String, i32> {
    let mut incr_binds = HashMap::<String, i32>::new();
    for (key, value) in binds.iter() {
        incr_binds.insert(key.to_string(), value + 1);
    };
    return incr_binds;
}

/// Compiles Expr and bindings to Instrs
/// Essentially an in order tree traversal with locally scoped variable bindings
fn compile_to_instrs(e: &Expr, binds: HashMap::<String, i32>) -> Vec<Instr> {
    let mut instrs = vec![];
    match e {
        Expr::Number(i) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*i))),
        Expr::Id(s) => match binds.get(s) {
                Some(v) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *v))),
                None => panic!("Unbound variable identifier {}", s),
            }
        Expr::UnOp(op, e) => 
            {
                // Subexpression evaluation
                instrs.extend(compile_to_instrs(e, binds.clone()));
                
                // Actual operation
                match op {
                    Op1::Add1 => instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1))),
                    Op1::Sub1 => instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1))),
                };
            },
        Expr::BinOp(op, e1, e2) => 
            {
                // Stack issues and subexpression evaluation
                instrs.extend(compile_to_instrs(e2, binds.clone())); 
                instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(WORD_SIZE)));
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, 1), Val::Reg(Reg::RAX)));
                instrs.extend(compile_to_instrs(e1, incr_binds(binds))); 
            
                // Actual opeartion
                match op {
                    Op2::Plus => instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, 1))), 
                    Op2::Minus => instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, 1))),
                    Op2::Times => instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, 1))),
                };

                // Cleanup
                instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(WORD_SIZE)))
        },
        Expr::Let(new_binds, e) => {
            let mut updated_binds = binds.clone();
            
            // Check for duplicates
            if new_binds.iter().map(|(x, y)| (x, y)).collect::<HashMap<&String, &Expr>>().len() != new_binds.len() {
                panic!("Duplicate binding")
            };

            // Evaluate and add all bindings
            for (key, val) in new_binds.iter() {
                instrs.extend(compile_to_instrs(val, updated_binds.clone()));
                instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(WORD_SIZE)));
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, 1), Val::Reg(Reg::RAX)));
                updated_binds = incr_binds(updated_binds);
                updated_binds.insert(key.to_string(), 1);
            }
            
            // Evaluate final expression
            instrs.extend(compile_to_instrs(e, updated_binds.clone()));    
            instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(WORD_SIZE * (new_binds.len() as i32))));
        },
    };
    return instrs;
}

/// Converts register or immediate value to assembly
fn val_to_str(v: &Val) -> String {
    let val = match v {
        Val::Reg(r) => match r {
            Reg::RAX => "rax",
            Reg::RSP => "rsp",
        },
        Val::Imm(i) => &i.to_string(),
        Val::RegOffset(r, off) => match r {
            Reg::RAX => &format!("[rax + {}]", WORD_SIZE * (off - 2)),
            Reg::RSP => &format!("[rsp + {}]", WORD_SIZE * (off - 2)),
        },
    };
    return val.to_string();
}

/// Converts instruction to assembly
/// Requires val_to_str for register conversions
fn instr_to_str(i: &Instr) -> String {
    let cmd = match i {
        Instr::IMov(v1, v2) => format!("mov {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IAdd(v1, v2) => format!("add {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ISub(v1, v2) => format!("sub {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IMul(v1, v2) => format!("imul {}, {}", val_to_str(v1), val_to_str(v2)),
    }; 
    return cmd;
}

/// Compiles Expr to assembly program
fn compile(e: &Expr) -> String {
    let original_binds = HashMap::<String, i32>::new();
    let instrs = compile_to_instrs(&e, original_binds);
    let program = instrs.iter().map(|instr| format!("\t{}\n", instr_to_str(instr))).collect::<Vec<String>>().concat();
    return program.to_string();
}

/// Converts function to assembly program
fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    let parsed = parse(&in_contents);
    match parsed {
        Ok(parsed) => {
            let expr = parse_expr(&parsed); 
            let result = compile(&expr);
            let asm_program = format!(
                "
                section .text
                global our_code_starts_here
                our_code_starts_here:
                {}\tret
                ",
                result
            );

            let mut out_file = File::create(out_name)?;
            out_file.write_all(asm_program.as_bytes())?;

            return Ok(());
        },
        _ => panic!("Invalid"),
    };
}
