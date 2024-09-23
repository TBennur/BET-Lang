use core::panic;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val), // mov dest, source
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
}

enum Variable {
    // Type(location on stack)
    Int(i64),
    Bool(i64),
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    // Equal,
    // Greater,
    // GreaterEqual,
    // Less,
    // LessEqual,
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    // Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    // If(Box<Expr>, Box<Expr>, Box<Expr>),
    // RepeatUntil(Box<Expr>, Box<Expr>),
    // Set(String, Box<Expr>),
    // Block(Vec<Expr>),
}

fn is_keyword(id: &String) -> bool {
    return vec![
        "true",
        "false",
        "input",
        "let",
        "add1",
        "sub1",
        "if",
        "block",
        "repeat-until",
    ]
    .contains(&id.as_str());
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        // atoms
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(id)) => match id.as_str() {
            "let" | "add1" | "sub1" => panic!("Invalid"),
            _ => Expr::Id(id.to_string()),
        },

        // vectors
        Sexp::List(vec) => match &vec[..] {
            // un ops (exactly two things in vec)
            [Sexp::Atom(S(op)), e] => Expr::UnOp(
                match op.as_str() {
                    "add1" => Op1::Add1,
                    "sub1" => Op1::Sub1,
                    _ => panic!("Invalid"),
                },
                Box::new(parse_expr(e)),
            ),

            // let expression
            // has the form let ((binding1), (binding2)) (in expression)
            [Sexp::Atom(S(op)), Sexp::List(bindings), finially] if op == "let" => {
                if bindings.len() == 0 {
                    panic!("Invalid")
                } else {
                    Expr::Let(
                        bindings
                            .into_iter()
                            .map(|element| match element {
                                Sexp::List(binding) => match &binding[..] {
                                    [Sexp::Atom(S(id)), e] => {
                                        if is_keyword(id) {
                                            panic!("Invalid")
                                        } else {
                                            (id.to_string(), parse_expr(e))
                                        }
                                    }
                                    _ => panic!("Invalid"),
                                },
                                _ => panic!("Invalid"),
                            })
                            .collect(),
                        Box::new(parse_expr(finially)),
                    )
                }
            }

            // bin ops (exactly three things in vec)
            [Sexp::Atom(S(op)), arg1, arg2] => Expr::BinOp(
                match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    _ => panic!("Invalid"),
                },
                Box::new(parse_expr(arg1)),
                Box::new(parse_expr(arg2)),
            ),

            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

const SIZEOF_I_64: i64 = 8; // size of an integer in bytes

#[derive(Clone, Copy)]
enum ExprType {
    Int,
    Bool,
}

fn type_check(e: &Expr, type_bindings: im::HashMap<String, ExprType>) -> ExprType {
    match e {
        Expr::Id(id) => match type_bindings.get(id) {
            None => panic!("Invalid"),
            Some(t) => *t,
        },
        Expr::Number(_) => ExprType::Int,

        Expr::UnOp(_, expr) => match type_check(expr, type_bindings) {
            ExprType::Int => ExprType::Int,
            _ => panic!("type mismatch"),
        },

        Expr::Let(bindings, finally) => {
            let mut curr_let_binding = type_bindings;
            let mut in_this_let: im::HashSet<String> = im::HashSet::new();

            for (id, exp) in bindings {
                // check for duplicates
                match in_this_let.insert(id.to_string()) {
                    None => (),
                    Some(_) => panic!("Duplicate binding"),
                };

                // typecheck the expression
                // panics if if doesn't typecheck
                let expr_type = type_check(exp, curr_let_binding.clone());

                // bind id to that type, allowing shadowing of different types
                curr_let_binding.insert(id.to_string(), expr_type);
            }

            // evaluate the type of the final expression after all the bindings
            type_check(finally, curr_let_binding)
        }

        Expr::BinOp(op2, expr, expr1) => match op2 {
            Op2::Plus => match type_check(expr, type_bindings.clone()) {
                ExprType::Int => match type_check(expr1, type_bindings) {
                    ExprType::Int => ExprType::Int,
                    _ => panic!("type mismatch"),
                },
                _ => panic!("type mismatch"),
            },
            Op2::Minus => match type_check(expr, type_bindings.clone()) {
                ExprType::Int => match type_check(expr1, type_bindings) {
                    ExprType::Int => ExprType::Int,
                    _ => panic!("type mismatch"),
                },
                _ => panic!("type mismatch"),
            },
            Op2::Times => match type_check(expr, type_bindings.clone()) {
                ExprType::Int => match type_check(expr1, type_bindings) {
                    ExprType::Int => ExprType::Int,
                    _ => panic!("type mismatch"),
                },
                _ => panic!("type mismatch"),
            },
        },
    }
}

fn compile_bin_op_to_instrs(
    op: &Op2,
    b: &Expr, // first arg
    a: &Expr, // second arg
    scope_bindings: im::HashMap<String, i64>,
    mut rsp_offset: i64,
) -> Vec<Instr> {
    let mut instr_to_compute_res: Vec<Instr> = vec![];

    // compute the value of a into RAX
    let instr_to_compute_a = compile_to_instrs(a, scope_bindings.clone(), rsp_offset);
    instr_to_compute_res.extend(instr_to_compute_a);

    // store that value on the stack
    rsp_offset -= SIZEOF_I_64; // get 4 bytes of space on the stack
    let a_rsp_offset = rsp_offset;
    instr_to_compute_res.push(Instr::IMov(
        Val::RegOffset(Reg::RSP, a_rsp_offset),
        Val::Reg(Reg::RAX),
    ));

    // this computes b, and stores it in RAX. since we adjusted rsp_offset,
    // our stored value of a is still at a_rsp_offset
    let inst_to_compute_b = compile_to_instrs(b, scope_bindings.clone(), rsp_offset);
    instr_to_compute_res.extend(inst_to_compute_b);

    // now we have `a` at the memory location RSP + a_rsp_offset, and `b` in RAX
    // use OP to compute OP(b <rax>, a <rsp + a_rsp_offset>)
    instr_to_compute_res.push(match op {
        Op2::Minus => Instr::ISub,
        Op2::Plus => Instr::IAdd,
        Op2::Times => Instr::IMul,
    }(
        Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, a_rsp_offset)
    ));

    instr_to_compute_res
}

/// Produces a vector of instructions which, when executed, result in the value
/// of the expression in RAX
/// rsp_offset is the next available position on rsp to be used for storing results
fn compile_to_instrs(
    e: &Expr,
    scope_bindings: im::HashMap<String, i64>,
    rsp_offset: i64,
) -> Vec<Instr> {
    // binding maps a identifier to a location in memory-- specifcally, an
    // offset from rsp, in bytes
    match e {
        // immediate values
        Expr::Number(x) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*x))],

        Expr::Id(identifier) => match scope_bindings.get(identifier) {
            None => panic!("{}", format!("Unbound variable identifier {}", identifier)),
            Some(offset) => vec![Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, *offset),
            )],
        },

        // unary ops
        Expr::UnOp(op, exp) => {
            let mut instructions = compile_to_instrs(exp, scope_bindings.clone(), rsp_offset);

            instructions.push(match op {
                Op1::Add1 => Instr::IAdd,
                Op1::Sub1 => Instr::ISub,
            }(Val::Reg(Reg::RAX), Val::Imm(1)));
            instructions
        }

        // binary ops: put op(b, a) in rax
        Expr::BinOp(op, b, a) => compile_bin_op_to_instrs(op, b, a, scope_bindings, rsp_offset),

        // let expression
        Expr::Let(bindings, final_expr) => {
            let mut instructions_to_compile_let: Vec<Instr> = vec![];
            let mut curr_rsp_offset = rsp_offset;
            let mut curr_let_binding = scope_bindings;
            let mut in_this_let: im::HashSet<String> = im::HashSet::new();
            // evaluate in order using lexical scoping
            for (id, exp) in bindings {
                // check for duplicates
                match in_this_let.insert(id.to_string()) {
                    None => (),
                    Some(_) => panic!("Duplicate binding"),
                };

                // compute the value of exp into RAX
                let code_to_eval_exp = compile_to_instrs(exp, curr_let_binding.clone(), rsp_offset);
                instructions_to_compile_let.extend(code_to_eval_exp);

                // store that value on the stack
                curr_rsp_offset -= SIZEOF_I_64; // get 4 bytes of space on the stack
                let id_rsp_offset = curr_rsp_offset;
                instructions_to_compile_let.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, id_rsp_offset),
                    Val::Reg(Reg::RAX),
                ));

                // bind id to that location on the stack (in doing so, to that result)
                curr_let_binding.insert(id.to_string(), id_rsp_offset);
            }

            // evaluate the final expression after all the bindings into RAX
            instructions_to_compile_let.extend(compile_to_instrs(
                final_expr,
                curr_let_binding,
                curr_rsp_offset,
            ));

            instructions_to_compile_let
        }
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("mov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("add {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("sub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("imul {}, {}", val_to_str(dst), val_to_str(src)),
    }
}

fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RSP => "rsp".to_string(),
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(r) => reg_to_str(r),
        Val::RegOffset(r, off) => format!("[{} + {}]", reg_to_str(r), off),
        Val::Imm(x) => format!("{}", x),
    }
}

fn compile(e: &Expr) -> String {
    let flag: u64 = match type_check(e, im::HashMap::new()) {
        ExprType::Int => 1,
        ExprType::Bool => 0,
    };

    let mut instrs = compile_to_instrs(e, im::HashMap::new(), 0)
        .into_iter()
        .map(|instr| format!("  {}", instr_to_str(&instr)))
        .collect::<Vec<String>>();

    instrs.push(format!("mov rsi, {}", flag));
    instrs.join("\n")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let parsed = &parse(&in_contents);
    let expr = match parsed {
        Ok(sexp) => parse_expr(sexp),
        Err(_) => panic!("Invalid"),
    };

    // println!("{:#?}", expr);

    // let instructions = compile_to_instrs(&expr, im::HashMap::new(), 0);
    // println!("{:#?}", instructions);

    let result = compile(&expr);

    // println!("Expected: {}", compute(&expr));

    let asm_program = format!(
        "
section .text

extern snek_print

global our_code_starts_here
our_code_starts_here:
{}
  mov rdi, rax
  call snek_print
  ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
