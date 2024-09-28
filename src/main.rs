use core::panic;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

#[derive(Debug)]
enum Function {
    SnekPrint,
    SnekError,
}

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
    RSI,
    RDI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val), // mov dest, source
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    BitwiseAnd(Val, Val),
    Compare(Val, Val),
    Call(Function),
    AddLabel(String),
    Jump(String),
    JumpGreater(String),
    JumpGreaterEqual(String),
    JumpEqual(String),
    JumpLessEqual(String),
    JumpLess(String),
    JumpOverflow(String),
    Ret,
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
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug)]
enum Expr {
    Number(i32),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    RepeatUntil(Box<Expr>, Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Input,
}

const SIZEOF_I_64: i32 = 8; // size of an integer in bytes

const ENTRYPOINT_LABEL: &str = "our_code_starts_here";
const OVERFLOW_LABEL: &str = "overflow";

fn is_keyword(id: &str) -> bool {
    return vec![
        "true",
        "false",
        "input",
        "let",
        "add1",
        "sub1",
        "set!",
        "if",
        "block",
        "repeat-until",
    ]
    .contains(&id);
}

fn id_to_string(id: &str) -> String {
    if is_keyword(id) {
        panic!("Invalid")
    }
    id.to_string()
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        // atoms
        Sexp::Atom(I(n)) => match <i32>::try_from(*n) {
            Ok(num) => Expr::Number(num),
            Err(_) => panic!("Invalid"),
        },
        Sexp::Atom(S(id)) => match id.as_str() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            "input" => Expr::Input,
            id => Expr::Id(id_to_string(id)),
        },

        // vectors
        Sexp::List(vec) => match &vec[..] {
            // block
            // has the form block <expr>+
            [Sexp::Atom(S(op)), exprns @ ..] if op == "block" => {
                if exprns.len() == 0 {
                    panic!("Invalid")
                } else {
                    Expr::Block(
                        exprns
                            .into_iter()
                            .map(|element| parse_expr(element))
                            .collect(),
                    )
                }
            }

            // set!
            // has the form set <name> <expr>
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), expr] if op == "set!" => {
                Expr::Set(id_to_string(name), Box::new(parse_expr(expr)))
            }

            // repeat-until
            // has the form repeat-until <expr> <expr>
            [Sexp::Atom(S(op)), body, stop_cond] if op == "repeat-until" => {
                Expr::RepeatUntil(Box::new(parse_expr(body)), Box::new(parse_expr(stop_cond)))
            }

            // if expression
            // has the form if <expr> <expr> <expr>
            [Sexp::Atom(S(op)), cond, then_case, else_case] if op == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(then_case)),
                Box::new(parse_expr(else_case)),
            ),

            // let expression
            // has the form let ((binding1), (binding2)) (in expression)
            [Sexp::Atom(S(op)), Sexp::List(bindings), finally] if op == "let" => {
                if bindings.len() == 0 {
                    panic!("Invalid")
                } else {
                    Expr::Let(
                        bindings
                            .into_iter()
                            .map(|element| match element {
                                Sexp::List(binding) => match &binding[..] {
                                    [Sexp::Atom(S(id)), e] => (id_to_string(id), parse_expr(e)),
                                    _ => panic!("Invalid"),
                                },
                                _ => panic!("Invalid"),
                            })
                            .collect(),
                        Box::new(parse_expr(finally)),
                    )
                }
            }

            // match bin & un ops after everything else, so we don't try to
            // match other "un" ops like, ex, set

            // bin ops (exactly three things in vec)
            [Sexp::Atom(S(op)), arg1, arg2] => Expr::BinOp(
                match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    "=" => Op2::Equal,
                    ">" => Op2::Greater,
                    ">=" => Op2::GreaterEqual,
                    "<" => Op2::Less,
                    "<=" => Op2::LessEqual,
                    _ => panic!("Invalid"),
                },
                Box::new(parse_expr(arg1)),
                Box::new(parse_expr(arg2)),
            ),

            // un ops (exactly two things in vec)
            [Sexp::Atom(S(op)), e] => Expr::UnOp(
                match op.as_str() {
                    "add1" => Op1::Add1,
                    "sub1" => Op1::Sub1,
                    s => panic!("Invalid {:?} {:?}", s, e),
                },
                Box::new(parse_expr(e)),
            ),

            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

#[derive(Clone, Copy, PartialEq)]
enum ExprType {
    Int,
    Bool,
}

fn type_check(e: &Expr, type_bindings: im::HashMap<String, ExprType>) -> ExprType {
    match e {
        Expr::Input => ExprType::Int,

        Expr::Boolean(_) => ExprType::Bool,

        Expr::Id(id) => match type_bindings.get(id) {
            None => panic!("Invalid"),
            Some(t) => *t,
        },
        Expr::Number(_) => ExprType::Int,

        Expr::UnOp(_, expr) => match type_check(expr, type_bindings) {
            ExprType::Int => ExprType::Int,
            _ => panic!("type mismatch"),
        },

        Expr::Set(name, new_value) => {
            // fails if the name isn't in scope
            let t1 = *match type_bindings.get(name) {
                Some(t1) => t1,
                None => panic!("Invalid"),
            };

            // can only "set" a variable to the same type within the current scope
            let t1_prime = type_check(new_value, type_bindings.clone());
            if t1_prime != t1 {
                panic!("type mismatch")
            }
            t1
        }

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

        Expr::BinOp(op2, a, b) => match op2 {
            // int * int => int
            Op2::Plus | Op2::Minus | Op2::Times => {
                if type_check(a, type_bindings.clone()) == ExprType::Int
                    && type_check(b, type_bindings.clone()) == ExprType::Int
                {
                    ExprType::Int
                } else {
                    panic!("type mismatch")
                }
            }

            // t * t => bool
            Op2::Equal => {
                if type_check(a, type_bindings.clone()) == type_check(b, type_bindings.clone()) {
                    ExprType::Bool
                } else {
                    panic!("type mismatch")
                }
            }

            // int * int => int
            Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                if type_check(a, type_bindings.clone()) == ExprType::Int
                    && type_check(b, type_bindings.clone()) == ExprType::Int
                {
                    ExprType::Bool
                } else {
                    panic!("type mismatch")
                }
            }
        },

        // bool * t2 * t2 => t2
        Expr::If(cond, val_if_true, val_if_false) => {
            // cond should typecheck to bool
            if type_check(cond, type_bindings.clone()) != ExprType::Bool {
                panic!("type mismatch");
            };

            let t2 = type_check(val_if_true, type_bindings.clone());
            let t2_prime = type_check(val_if_false, type_bindings.clone());
            if t2 == t2_prime {
                t2
            } else {
                panic!("type mismatch")
            }
        }

        // t1 * t2 => t1
        Expr::RepeatUntil(body, stop_cond) => {
            // stop_cond must be a bool
            if type_check(stop_cond, type_bindings.clone()) != ExprType::Bool {
                panic!("type mismatch")
            }

            // repeat-until evaluates to the body (once stop_cond is true)
            type_check(body, type_bindings.clone())
        }

        Expr::Block(expns) => {
            if expns.len() == 0 {
                panic!("invalid")
            }
            let mut final_type = ExprType::Int;
            for expr in expns {
                // typecheck each expression in the block
                final_type = type_check(expr, type_bindings.clone());
            }
            // since block evaluates to the type of the last expression, that's the type of the block
            final_type
        }
    }
}

fn increment_counter(counter: &mut i64) -> i64 {
    *counter += 1;
    *counter
}

fn generate_label(label_counter: i64) -> String {
    return format!("{}_label_{}", ENTRYPOINT_LABEL, label_counter).to_string();
}

fn compile_bin_op_to_instrs(
    op: &Op2,
    b: &Expr, // first arg
    a: &Expr, // second arg
    scope_bindings: im::HashMap<String, i32>,
    mut rsp_offset: i32,
    label_counter: &mut i64,
) -> Vec<Instr> {
    let mut instr_to_compute_res: Vec<Instr> = vec![];

    // compute the value of a into RAX
    let instr_to_compute_a =
        compile_to_instrs(a, scope_bindings.clone(), rsp_offset, label_counter);
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
    let inst_to_compute_b = compile_to_instrs(b, scope_bindings.clone(), rsp_offset, label_counter);
    instr_to_compute_res.extend(inst_to_compute_b);

    // now we have `a` at the memory location RSP + a_rsp_offset, and `b` in RAX
    // use OP to compute OP(b <rax>, a <rsp + a_rsp_offset>)

    match op {
        Op2::Minus | Op2::Plus | Op2::Times => {
            instr_to_compute_res.push(match op {
                Op2::Minus => Instr::ISub,
                Op2::Plus => Instr::IAdd,
                Op2::Times => Instr::IMul,
                _ => panic!("Unexpected"),
            }(
                Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, a_rsp_offset)
            ));
            // Do Overflow Checking
            instr_to_compute_res.push(Instr::JumpOverflow(String::from(OVERFLOW_LABEL)));
        }
        _ => {
            instr_to_compute_res.push(Instr::Compare(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            ));

            let label_true = generate_label(increment_counter(label_counter));
            let label_finish = generate_label(increment_counter(label_counter));

            instr_to_compute_res.push(match op {
                Op2::Greater => Instr::JumpGreater,
                Op2::GreaterEqual => Instr::JumpGreaterEqual,
                Op2::Equal => Instr::JumpEqual,
                Op2::LessEqual => Instr::JumpLessEqual,
                Op2::Less => Instr::JumpLess,
                _ => panic!("Unexpected"),
            }(label_true.clone()));
            instr_to_compute_res.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));
            instr_to_compute_res.push(Instr::Jump(label_finish.clone()));
            instr_to_compute_res.push(Instr::AddLabel(label_true.clone()));
            instr_to_compute_res.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            instr_to_compute_res.push(Instr::Jump(label_finish.clone()));
            instr_to_compute_res.push(Instr::AddLabel(label_finish.clone()));
        }
    };

    instr_to_compute_res
}

/// Produces a vector of instructions which, when executed, result in the value
/// of the expression in RAX
/// 
/// `rsp_offset` is the next available position on rsp to be used for storing results
fn compile_to_instrs(
    e: &Expr,
    scope_bindings: im::HashMap<String, i32>,
    rsp_offset: i32,
    label_counter: &mut i64,
) -> Vec<Instr> {
    // binding maps a identifier to a location in memory-- specifcally, an
    // offset from rsp, in bytes
    match e {
        // immediate values
        Expr::Input => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))],
        Expr::Boolean(b) => match b {
            false => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0))],
            true => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
        },
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
            let mut instructions =
                compile_to_instrs(exp, scope_bindings.clone(), rsp_offset, label_counter);

            instructions.push(match op {
                Op1::Add1 => Instr::IAdd,
                Op1::Sub1 => Instr::ISub,
            }(Val::Reg(Reg::RAX), Val::Imm(1)));
            instructions
        }

        // binary ops: put op(b, a) in rax
        Expr::BinOp(op, b, a) => {
            compile_bin_op_to_instrs(op, b, a, scope_bindings, rsp_offset, label_counter)
        }

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
                let code_to_eval_exp =
                    compile_to_instrs(exp, curr_let_binding.clone(), rsp_offset, label_counter);
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
                label_counter,
            ));

            instructions_to_compile_let
        }

        Expr::If(expr, expr1, expr2) => {
            let mut instructions_to_compile_if: Vec<Instr> = vec![];

            // Conditional Expression
            let code_to_eval_expr =
                compile_to_instrs(expr, scope_bindings.clone(), rsp_offset, label_counter);
            instructions_to_compile_if.extend(code_to_eval_expr);
            instructions_to_compile_if.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(1)));
            let left_label = generate_label(increment_counter(label_counter));

            instructions_to_compile_if.push(Instr::JumpEqual(left_label.clone()));

            // Right Branch
            let code_to_eval_expr2 =
                compile_to_instrs(expr2, scope_bindings.clone(), rsp_offset, label_counter);
            instructions_to_compile_if.extend(code_to_eval_expr2);

            let if_finish_label = generate_label(increment_counter(label_counter));
            instructions_to_compile_if.push(Instr::Jump(if_finish_label.clone()));

            // Left Branch
            instructions_to_compile_if.push(Instr::AddLabel(left_label.clone()));
            let code_to_eval_expr1 =
                compile_to_instrs(expr1, scope_bindings.clone(), rsp_offset, label_counter);
            instructions_to_compile_if.extend(code_to_eval_expr1);

            // Finish
            instructions_to_compile_if.push(Instr::AddLabel(if_finish_label.clone()));

            instructions_to_compile_if
        }

        Expr::Set(identifier, expr) => {
            // get the rsp offset where this variable is stored
            let id_rsp_offset = match scope_bindings.get(identifier) {
                None => panic!("{}", format!("Unbound variable identifier {}", identifier)),
                Some(offset) => *offset,
            };

            // get the asm instructions to evaluate `expr` into rax
            let mut instructions_to_compile_set: Vec<Instr> = vec![];
            instructions_to_compile_set.extend(compile_to_instrs(
                expr,
                scope_bindings.clone(),
                rsp_offset,
                label_counter,
            ));

            // add instruction to update value of binding
            instructions_to_compile_set.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, id_rsp_offset),
                Val::Reg(Reg::RAX),
            ));

            // don't change the value in rax since the set expression evaluates to the new value

            instructions_to_compile_set
        }
        Expr::Block(block) => {
            if block.len() == 0 {
                panic!("Invalid");
            }

            let mut instructions_to_compile_block: Vec<Instr> = vec![];

            // compile each expression
            for exp in block {
                instructions_to_compile_block.extend(compile_to_instrs(
                    exp,
                    scope_bindings.clone(),
                    rsp_offset,
                    label_counter,
                ));
            }

            // value of last expression in block is already in rax, so done
            instructions_to_compile_block
        }

        Expr::RepeatUntil(body, stop_cond) => {
            let mut instructions_to_compile_repeat_until: Vec<Instr> = vec![];
            
            // add body label
            let body_label = generate_label(increment_counter(label_counter));
            instructions_to_compile_repeat_until.push(Instr::AddLabel(body_label.clone()));

            // compile the body
            instructions_to_compile_repeat_until.extend(compile_to_instrs(body, scope_bindings.clone(), rsp_offset, label_counter));

            // push value of body (rax) onto stack
            let id_rsp_offset = rsp_offset - SIZEOF_I_64;
            instructions_to_compile_repeat_until.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, id_rsp_offset),
                Val::Reg(Reg::RAX),
            ));

            // evaluate the stop condition, which moves its result into rax
            instructions_to_compile_repeat_until.extend(compile_to_instrs(stop_cond, scope_bindings.clone(), id_rsp_offset, label_counter));

            // compare the value of the stop_condition to 0; jump if it's equal to 0, ie, not stopped
            instructions_to_compile_repeat_until.push(Instr::Compare(Val::Reg(Reg::RAX),Val::Imm(0)));
            
            // jump to body label if equal to 0, ie, stopped
            instructions_to_compile_repeat_until.push(Instr::JumpEqual(body_label.clone()));
            
            // move the stored value to rax
            instructions_to_compile_repeat_until.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, id_rsp_offset)));            
        
            instructions_to_compile_repeat_until
        },
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("\tmov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("\tadd {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("\tsub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("\timul {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::BitwiseAnd(dst, src) => format!("\tand {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Compare(dst, src) => format!("\tcmp {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Call(function) => format!("\tcall {}", fn_to_str(function)),
        Instr::AddLabel(label) => format!("{}:", label.to_string()),
        Instr::Jump(label) => format!("\tjmp {}", label.to_string()),
        Instr::JumpGreater(label) => format!("\tjg {}", label.to_string()),
        Instr::JumpGreaterEqual(label) => format!("\tjge {}", label.to_string()),
        Instr::JumpEqual(label) => format!("\tje {}", label.to_string()),
        Instr::JumpLessEqual(label) => format!("\tjle {}", label.to_string()),
        Instr::JumpLess(label) => format!("\tjl {}", label.to_string()),
        Instr::JumpOverflow(label) => format!("\tjo {}", label.to_string()),
        Instr::Ret => "\tret".to_string(),
    }
}

fn fn_to_str(f: &Function) -> String {
    match f {
        Function::SnekPrint => "snek_print".to_string(),
        Function::SnekError => "snek_error".to_string(),
    }
}

fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RSP => "rsp".to_string(),
        Reg::RSI => "rsi".to_string(),
        Reg::RDI => "rdi".to_string(),
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
    let flag: i32 = match type_check(e, im::HashMap::new()) {
        ExprType::Int => 1,
        ExprType::Bool => 0,
    };

    // add the label for our code
    let mut instrs = vec![Instr::AddLabel(ENTRYPOINT_LABEL.to_string())];
    
    // compile the instructions which evaluate the expression, loading the result into rax
    let mut label_counter: i64 = 0;
    instrs.extend(compile_to_instrs(
        e,
        im::HashMap::new(),
        0,
        &mut label_counter,
    ));

    // on a success, we fall through to here, and snek print is called
    // move type flag (value given by typecheck) to rsi
    instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(flag)));

    // move the result (stored in rax) to rdi
    // instrs.push(Instr::BitwiseAnd(Val::Reg(Reg::RSP), Val::Imm(-16)));
    instrs.push(Instr::BitwiseAnd(Val::Reg(Reg::RSP), Val::Imm(-16))); // Force Alignment
    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
    instrs.push(Instr::Call(Function::SnekPrint));
    instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(8))); // Reset Alignment
    instrs.push(Instr::Ret);
    // call SnekPrint (takes in rdi, the result, and rsi, the type)
    // instrs.push(Instr::Call(Function::SnekPrint));

    // return after SnekPrint is called

    // a runtime error causes us to jump here before reaching the SnekPrint call
    instrs.push(Instr::AddLabel(OVERFLOW_LABEL.to_string()));
    instrs.push(Instr::Call(Function::SnekError));
    instrs.push(Instr::Ret);

    instrs
        .into_iter()
        .map(|instr| format!("  {}", instr_to_str(&instr)))
        .collect::<Vec<String>>()
        .join("\n")
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

    let result = compile(&expr);

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
