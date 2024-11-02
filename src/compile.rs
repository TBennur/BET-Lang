use im::HashMap;

use crate::consts::*;
use crate::semantics::{struct_name_to_type_enum, struct_type_enum_to_name, STRUCT_NUM_TO_NAME};
use crate::structs::*;
use crate::typecheck::type_check_prog;

/**
 * Helper Function
 */

pub fn type_to_flag(t: ExprType) -> i32 {
    match t {
        ExprType::Int => INT_TYPE_FLAG,
        ExprType::Bool => BOOL_TYPE_FLAG,
        ExprType::StructPointer(i) => i,
    }
}

fn increment_counter(counter: &mut i64) -> i64 {
    *counter += 1;
    *counter
}

fn generate_label(label_name: &String, label_counter: i64) -> String {
    return format!("{}_label_{}", label_name, label_counter);
}

fn compute_aligned_rsp_offset(rsp_offset: i32) -> i32 {
    return (rsp_offset / 16) * 16 - 8;
}

/**
 * To String Functions
 */

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("\tmov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("\tadd {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("\tsub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("\timul {}, {}", val_to_str(dst), val_to_str(src)),
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
        Instr::Lea(dst, src) => format!("\tlea {}, [rel {}]", val_to_str(dst), val_to_str(src)),
    }
}

fn fn_to_str(f: &FunctionLabel) -> String {
    match f {
        FunctionLabel::SnekPrint => "snek_print".to_string(),
        FunctionLabel::SnekError => "snek_error".to_string(),
        FunctionLabel::Custom(name) => name.to_string(),
    }
}

fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RBX => "rbx".to_string(),
        Reg::RDX => "rdx".to_string(),
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
        Val::Global(global_name) => format!("{}", global_name),
    }
}

/**
 * Compilation Functions
 */

fn compile_bin_op_to_instrs(
    op: &Op2,
    b: &TypedExpr, // first arg
    a: &TypedExpr, // second arg
    scope_bindings: im::HashMap<String, i32>,
    struct_layouts: &im::HashMap<String, StructLayout>,
    mut rsp_offset: i32,
    label_counter: &mut i64,
    label_name: &String,
) -> Vec<Instr> {
    let mut instr_to_compute_res: Vec<Instr> = vec![];

    // compute the value of a into RAX
    let instr_to_compute_a = compile_expr_to_instrs(
        a,
        scope_bindings.clone(),
        &struct_layouts,
        rsp_offset,
        label_counter,
        label_name,
    );
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
    let inst_to_compute_b = compile_expr_to_instrs(
        b,
        scope_bindings.clone(),
        &struct_layouts,
        rsp_offset,
        label_counter,
        label_name,
    );
    instr_to_compute_res.extend(inst_to_compute_b);

    // now we have `a` at the memory location RSP + a_rsp_offset, and `b` in RAX
    // use OP to compute OP(b <rax>, a <rsp + a_rsp_offset>)

    match op {
        Op2::Minus | Op2::Plus | Op2::Times => {
            instr_to_compute_res.push(match op {
                Op2::Minus => Instr::ISub,
                Op2::Plus => Instr::IAdd,
                Op2::Times => Instr::IMul,
                _ => panic!("Unexpected: This should never be called"),
            }(
                Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, a_rsp_offset)
            ));
            // Do Overflow Checking

            instr_to_compute_res.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
            instr_to_compute_res.push(Instr::JumpOverflow(String::from(ERROR_LABEL)));
        }
        _ => {
            instr_to_compute_res.push(Instr::Compare(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            ));

            let label_true = generate_label(label_name, increment_counter(label_counter));
            let label_finish = generate_label(label_name, increment_counter(label_counter));

            instr_to_compute_res.push(match op {
                Op2::Greater => Instr::JumpGreater,
                Op2::GreaterEqual => Instr::JumpGreaterEqual,
                Op2::Equal => Instr::JumpEqual,
                Op2::LessEqual => Instr::JumpLessEqual,
                Op2::Less => Instr::JumpLess,
                _ => panic!("Unexpected: This should never be called"),
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
/// once all instructions have been executed, RSP should have the same value as before any executed
fn compile_expr_to_instrs(
    e: &TypedExpr,
    scope_bindings: im::HashMap<String, i32>,
    struct_layouts: &im::HashMap<String, StructLayout>,
    rsp_offset: i32,
    label_counter: &mut i64,
    label_name: &String,
) -> Vec<Instr> {
    // binding maps a identifier to a location in memory-- specifcally, an
    // offset from rsp, in bytes
    match e {
        TypedExpr::RDInput => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))],

        // immediate values
        TypedExpr::Boolean(b) => match b {
            false => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0))],
            true => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))],
        },
        TypedExpr::Number(x) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*x))],

        // input will be passed to main as any other argument: on the stack
        TypedExpr::Input => match scope_bindings.get(&"input".to_string()) {
            None => panic!("Unbound variable identifier {:?}", "input"),
            Some(offset) => vec![Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, *offset),
            )],
        },

        TypedExpr::Id(_, identifier) => match scope_bindings.get(identifier) {
            None => panic!("Unbound variable identifier {:?}", identifier),
            Some(offset) => vec![Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, *offset),
            )],
        },

        // unary ops
        TypedExpr::UnOp(t, op, exp) => {
            let mut instructions = compile_expr_to_instrs(
                exp,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            );

            match op {
                Op1::Add1 => instructions.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1))),
                Op1::Sub1 => instructions.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1))),
                Op1::Print => {
                    let flag = type_to_flag(*t);

                    instructions.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(flag)));
                    instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))); // load val into rdi

                    instructions.push(Instr::IAdd(
                        Val::Reg(Reg::RSP),
                        Val::Imm(compute_aligned_rsp_offset(rsp_offset)),
                    )); // Reset Alignment
                    instructions.push(Instr::Lea(
                        Val::Reg(Reg::RDX),
                        Val::Global("structHashmap".to_string()),
                    ));
                    instructions.push(Instr::Call(FunctionLabel::SnekPrint));

                    instructions.push(Instr::ISub(
                        Val::Reg(Reg::RSP),
                        Val::Imm(compute_aligned_rsp_offset(rsp_offset)),
                    )); // Reset Alignment
                }
            };
            instructions
        }

        // binary ops: put op(b, a) in rax
        TypedExpr::BinOp(_, op, b, a) => compile_bin_op_to_instrs(
            op,
            b,
            a,
            scope_bindings,
            &struct_layouts,
            rsp_offset,
            label_counter,
            label_name,
        ),

        // let expression
        TypedExpr::Let(_, bindings, final_expr) => {
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
                let code_to_eval_exp = compile_expr_to_instrs(
                    exp,
                    curr_let_binding.clone(),
                    &struct_layouts,
                    curr_rsp_offset,
                    label_counter,
                    label_name,
                );
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
            instructions_to_compile_let.extend(compile_expr_to_instrs(
                final_expr,
                curr_let_binding,
                &struct_layouts,
                curr_rsp_offset,
                label_counter,
                label_name,
            ));

            instructions_to_compile_let
        }

        TypedExpr::If(_, expr, expr1, expr2) => {
            let mut instructions_to_compile_if: Vec<Instr> = vec![];

            // Conditional Expression
            let code_to_eval_expr = compile_expr_to_instrs(
                expr,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            );
            instructions_to_compile_if.extend(code_to_eval_expr);
            instructions_to_compile_if.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(1)));
            let left_label = generate_label(label_name, increment_counter(label_counter));

            instructions_to_compile_if.push(Instr::JumpEqual(left_label.clone()));

            // Right Branch
            let code_to_eval_expr2 = compile_expr_to_instrs(
                expr2,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            );
            instructions_to_compile_if.extend(code_to_eval_expr2);

            let if_finish_label = generate_label(label_name, increment_counter(label_counter));
            instructions_to_compile_if.push(Instr::Jump(if_finish_label.clone()));

            // Left Branch
            instructions_to_compile_if.push(Instr::AddLabel(left_label.clone()));
            let code_to_eval_expr1 = compile_expr_to_instrs(
                expr1,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            );
            instructions_to_compile_if.extend(code_to_eval_expr1);

            // Finish
            instructions_to_compile_if.push(Instr::AddLabel(if_finish_label.clone()));

            instructions_to_compile_if
        }

        TypedExpr::Set(_, identifier, expr) => {
            // get the rsp offset where this variable is stored
            let id_rsp_offset = match scope_bindings.get(identifier) {
                None => panic!("Unbound variable identifier {:?}", identifier),
                Some(offset) => *offset,
            };

            // get the asm instructions to evaluate `expr` into rax
            let mut instructions_to_compile_set: Vec<Instr> = vec![];
            instructions_to_compile_set.extend(compile_expr_to_instrs(
                expr,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            ));

            // add instruction to update value of binding
            instructions_to_compile_set.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, id_rsp_offset),
                Val::Reg(Reg::RAX),
            ));

            // don't change the value in rax since the set expression evaluates to the new value

            instructions_to_compile_set
        }
        TypedExpr::Block(_, block) => {
            if block.len() == 0 {
                panic!("Invalid: Empty Block");
            }

            let mut instructions_to_compile_block: Vec<Instr> = vec![];

            // compile each expression
            for exp in block {
                instructions_to_compile_block.extend(compile_expr_to_instrs(
                    exp,
                    scope_bindings.clone(),
                    &struct_layouts,
                    rsp_offset,
                    label_counter,
                    label_name,
                ));
            }

            // value of last expression in block is already in rax, so done
            instructions_to_compile_block
        }

        TypedExpr::RepeatUntil(_, body, stop_cond) => {
            let mut instructions_to_compile_repeat_until: Vec<Instr> = vec![];

            // add body label
            let body_label = generate_label(label_name, increment_counter(label_counter));
            instructions_to_compile_repeat_until.push(Instr::AddLabel(body_label.clone()));

            // compile the body
            instructions_to_compile_repeat_until.extend(compile_expr_to_instrs(
                body,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            ));

            // push value of body (rax) onto stack
            let id_rsp_offset = rsp_offset - SIZEOF_I_64;
            instructions_to_compile_repeat_until.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, id_rsp_offset),
                Val::Reg(Reg::RAX),
            ));

            // evaluate the stop condition, which moves its result into rax
            instructions_to_compile_repeat_until.extend(compile_expr_to_instrs(
                stop_cond,
                scope_bindings.clone(),
                &struct_layouts,
                id_rsp_offset,
                label_counter,
                label_name,
            ));

            // compare the value of the stop_condition to 0; jump if it's equal to 0, ie, not stopped
            instructions_to_compile_repeat_until
                .push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(0)));

            // jump to body label if equal to 0, ie, stopped
            instructions_to_compile_repeat_until.push(Instr::JumpEqual(body_label.clone()));

            // move the stored value to rax
            instructions_to_compile_repeat_until.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, id_rsp_offset),
            ));

            instructions_to_compile_repeat_until
        }

        TypedExpr::Call(_ret_type, fun_name, args) => {
            /*
            foo(a, b) =>
            |    a        | <-- RSP + 16
            |    b        | <-- RSP + 8
            | return addr | <-- RSP
            */

            let mut instructions = Vec::new();
            let mut curr_rsp_offset = rsp_offset;

            // what rsp would be after pushing args
            let unadjusted_call_rsp = rsp_offset - (SIZEOF_I_64 * args.len() as i32);

            // preemptively adjust rsp so that it will 8 byte aligned, mod 16, after pushing args
            curr_rsp_offset +=
                compute_aligned_rsp_offset(unadjusted_call_rsp) - unadjusted_call_rsp;

            // evaluate the arguments, pushing each onto the stack
            for arg in args {
                let instr_to_eval_arg = compile_expr_to_instrs(
                    arg,
                    scope_bindings.clone(),
                    &struct_layouts,
                    curr_rsp_offset,
                    label_counter,
                    label_name,
                );
                instructions.extend(instr_to_eval_arg);

                // store this argument on the stack
                curr_rsp_offset -= SIZEOF_I_64;
                instructions.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, curr_rsp_offset),
                    Val::Reg(Reg::RAX),
                ));
            }

            // update RSP for the function call
            instructions.push(Instr::IAdd(
                Val::Reg(Reg::RSP),
                Val::Imm(compute_aligned_rsp_offset(curr_rsp_offset)),
            )); // Reset Alignment

            // call the function
            instructions.push(Instr::Call(FunctionLabel::Custom(fun_name.to_string())));

            instructions.push(Instr::ISub(
                Val::Reg(Reg::RSP),
                Val::Imm(compute_aligned_rsp_offset(curr_rsp_offset)),
            )); // Reset Alignment

            instructions
        }
        TypedExpr::Null(_) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0))],
        TypedExpr::Alloc(expr_type) => {
            let mut instructions = Vec::new();
            let type_string = match expr_type {
                ExprType::Bool => {
                    panic!("Unexpected: Attempted to create a Bool pointer. This should not happen")
                }
                ExprType::Int => {
                    panic!("Unexpected: Attempted to create an Int pointer. This should not happen")
                }
                ExprType::StructPointer(n) => {
                    let num_to_name_map = STRUCT_NUM_TO_NAME.lock().unwrap();
                    let res = num_to_name_map.get(&n);
                    match res {
                        Some(s) => s.to_string(),
                        None => panic!(
                            "Unexpected: Broken structure enumeration. This should not happen"
                        ),
                    }
                }
            };
            let size = match struct_layouts.get(&type_string) {
                Some(StructLayout::Layout(layout_dict)) => layout_dict.len() as i32,
                None => panic!("Unexpected: Broken structure dictionary. This should not happen"),
            };

            let alloc_size = SIZEOF_I_64 * size;

            // check how many bytes we've allocated (stored in RDX)

            // logic: if (RDX + alloc_size) > BUFFER_SIZE, go to ERROR_LABEL
            instructions.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // rax := num bytes allocated
            instructions.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(alloc_size))); // rax := num bytes allocated + num bytes needed
            instructions.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(BUFFER_SIZE))); // if (num bytes allocated + num bytes needed) > BUFFER_SIZE:
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2)));
            instructions.push(Instr::JumpGreater(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            // we have enough room; get the address of bump_array[rbx]
            instructions.push(Instr::Lea(
                Val::Reg(Reg::RAX),
                Val::Global(BUFFER_NAME.to_string()),
            )); // rax := &array
            instructions.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // rax := &array + rbx

            // update the total bytes allocated, so that the next alloc points to next free addrr
            instructions.push(Instr::IAdd(Val::Reg(Reg::RBX), Val::Imm(alloc_size))); // rbx := num bytes allocated + num bytes needed
            instructions
        }
        TypedExpr::Update(_, typed_expr, field_name, typed_expr_val) => {
            let mut instructions = Vec::new();
            let expr_type = extract_type(typed_expr);
            let type_string = match expr_type {
                ExprType::Bool => {
                    panic!("Unexpected: Attempted to access a Bool pointer. This should not happen")
                }
                ExprType::Int => {
                    panic!("Unexpected: Attempted to access an Int pointer. This should not happen")
                }
                ExprType::StructPointer(n) => {
                    let num_to_name_map = STRUCT_NUM_TO_NAME.lock().unwrap();
                    let res = num_to_name_map.get(&n);
                    match res {
                        Some(s) => s.to_string(),
                        None => panic!(
                            "Unexpected: Broken structure enumeration. This should not happen"
                        ),
                    }
                }
            };
            let offset = match struct_layouts.get(&type_string) {
                Some(StructLayout::Layout(layout_dict)) => match layout_dict.get(field_name) {
                    Some(i) => i,
                    None => panic!("Unexpected: Missing field. This should not happen"),
                },
                None => panic!("Unexpected: Broken structure dictionary. This should not happen"),
            };

            let instr_to_get_val = compile_expr_to_instrs(
                typed_expr_val,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            );
            instructions.extend(instr_to_get_val);
            let cur_rsp_offset = rsp_offset - SIZEOF_I_64;
            instructions.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, cur_rsp_offset),
                Val::Reg(Reg::RAX),
            ));

            let instr_to_get_struct = compile_expr_to_instrs(
                typed_expr,
                scope_bindings.clone(),
                &struct_layouts,
                cur_rsp_offset,
                label_counter,
                label_name,
            );
            instructions.extend(instr_to_get_struct);

            instructions.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(0))); // if val is null
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
            instructions.push(Instr::JumpEqual(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            instructions.push(Instr::IMov(
                Val::Reg(Reg::RDI),
                Val::RegOffset(Reg::RSP, cur_rsp_offset),
            ));
            instructions.push(Instr::IMov(
                Val::RegOffset(Reg::RAX, SIZEOF_I_64 * offset),
                Val::Reg(Reg::RDI),
            ));

            instructions
        }
        TypedExpr::Lookup(_, typed_expr, field_name) => {
            let mut instructions = Vec::new();
            let expr_type = extract_type(typed_expr);
            let type_string = match expr_type {
                ExprType::Bool => {
                    panic!("Unexpected: Attempted to access a Bool pointer. This should not happen")
                }
                ExprType::Int => {
                    panic!("Unexpected: Attempted to access an Int pointer. This should not happen")
                }
                ExprType::StructPointer(n) => {
                    let num_to_name_map = STRUCT_NUM_TO_NAME.lock().unwrap();
                    let res = num_to_name_map.get(&n);
                    match res {
                        Some(s) => s.to_string(),
                        None => panic!(
                            "Unexpected: Broken structure enumeration. This should not happen"
                        ),
                    }
                }
            };
            let offset = match struct_layouts.get(&type_string) {
                Some(StructLayout::Layout(layout_dict)) => match layout_dict.get(field_name) {
                    Some(i) => i,
                    None => panic!("Unexpected: Missing field. This should not happen"),
                },
                None => panic!("Unexpected: Broken structure dictionary. This should not happen"),
            };

            let instr_to_get_struct = compile_expr_to_instrs(
                typed_expr,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            );
            // struct ptr is now in rax
            instructions.extend(instr_to_get_struct);

            // if struct ptr is null, die
            instructions.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(0))); // if val is null
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
            instructions.push(Instr::JumpEqual(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            // didn't die, so struct ptr isn't null, so access
            instructions.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RAX, SIZEOF_I_64 * offset),
            ));

            instructions
        }
    }
}

fn compile_fn(f: &TypedFunction, struct_layouts: &HashMap<String, StructLayout>) -> Vec<Instr> {
    let TypedFunction::Fun(fun_name, FunSignature::Sig(_ret_type, args), typed_body) = f;

    // add the label for our code
    let mut instrs = vec![Instr::AddLabel(fun_name.to_string())];

    if fun_name.clone() == ENTRYPOINT_LABEL {
        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0)))
    }

    let init_rsp_offset = 0; // the body of a function expects RSP has been adjusted
    let mut label_counter = 0; // function names should be unique, so can reset counter

    // prep arguments for our function to access
    let mut scope_bindings = im::HashMap::new();

    /*
    foo(a, b) =>
    |    a        | <-- RSP + 16
    |    b        | <-- RSP + 8
    | return addr | <-- RSP
    */

    // iterate over args in reverse order, since later args are closer to RSP
    let mut arg_offset_above_rsp = 0;
    for (_, arg_name) in args.iter().rev() {
        arg_offset_above_rsp += SIZEOF_I_64;
        scope_bindings.insert(arg_name.to_string(), arg_offset_above_rsp);
    }

    // RSP should be the same after running these instr as it was before
    instrs.extend(compile_expr_to_instrs(
        typed_body,
        scope_bindings,
        &struct_layouts,
        init_rsp_offset,
        &mut label_counter,
        fun_name,
    ));

    // return execution
    instrs.push(Instr::Ret);

    instrs
}

fn serialize_struct_layouts(
    struct_sigs: &HashMap<String, StructSignature>, // to know the types of fields
    struct_layouts: &HashMap<String, StructLayout>, // to know the offsets of fields
) -> String {
    let mut res_vec: Vec<Vec<String>> = Vec::new();
    for (struct_name, StructLayout::Layout(struct_layout)) in struct_layouts {
        let StructSignature::Sig(struct_field_types) = match struct_sigs.get(struct_name) {
            Some(sig) => sig,
            None => unreachable!("Struct name is in struct layouts but not struct sigs."),
        };

        let mut subres_vec: Vec<String> = Vec::new(); // [struct struct_type_enum, struct_name, offset_1, field_name_1, type_1 ...]
        subres_vec.push("struct".to_string()); // filler so that we can chunk into 3
        subres_vec.push(struct_name_to_type_enum(struct_name).to_string());
        subres_vec.push(struct_name.to_string());

        let mut struct_layout: Vec<(&i32, &String)> = struct_layout
            .iter()
            .map(|(fname, offset)| (offset, fname))
            .collect();
        struct_layout.sort_by_key(|(offset, _fname)| (*offset));
        for (i, (offset, field_name)) in struct_layout.iter().enumerate() {
            subres_vec.push(offset.to_string());
            subres_vec.push(field_name.to_string());
            subres_vec.push(match struct_field_types.get(i) {
                Some((expr_type, _field_name)) => match expr_type {
                    ExprType::Int => "int".to_string(),
                    ExprType::Bool => "bool".to_string(),
                    ExprType::StructPointer(struct_type_enum) => {
                        match struct_type_enum_to_name(*struct_type_enum) {
                            Some(s) => s,
                            None => unreachable!(),
                        }
                    }
                },
                None => unreachable!(),
            });
        }
        res_vec.push(subres_vec);
    }

    let res_vec: Vec<String> = res_vec.into_iter().map(|subres| subres.join(".")).collect();
    res_vec.join(",")
}

pub fn compile_prog(p: &Prog) -> String {
    let TypedProg::Program(body_type, struct_sigs, struct_layouts, typed_funs, typed_e) =
        type_check_prog(p);

    let mut all_instrs = Vec::new();

    for typed_fun in typed_funs {
        all_instrs.extend(compile_fn(&typed_fun, &struct_layouts));
    }

    // compile program body, with the label __main
    let main = TypedFunction::Fun(
        MAIN_LABEL.to_string(),
        FunSignature::Sig(body_type, vec![(ExprType::Int, "input".to_string())]), // input: int -> body_type
        typed_e,
    );
    all_instrs.extend(compile_fn(&main, &struct_layouts));

    // compile the entrypoint, which loads RDI (input) onto the stack, then calls __main
    let entrypoint = TypedFunction::Fun(
        // (print (main input : int ) : body_type ) : body_type
        ENTRYPOINT_LABEL.to_string(),
        FunSignature::Sig(body_type, Vec::new()), // unit -> body_type
        TypedExpr::UnOp(
            body_type,
            Op1::Print,
            Box::new(TypedExpr::Call(
                body_type,
                MAIN_LABEL.to_string(),
                vec![TypedExpr::RDInput],
            )),
        ),
    );
    all_instrs.extend(compile_fn(&entrypoint, &struct_layouts));

    // a runtime error causes us to jump here before reaching the SnekPrint call
    // since this function doesn't typecheck in the current type system, write it in ASM
    all_instrs.push(Instr::AddLabel(ERROR_LABEL.to_string()));
    all_instrs.push(Instr::Call(FunctionLabel::SnekError));
    all_instrs.push(Instr::Ret);

    let instrs_string = all_instrs
        .into_iter()
        .map(|instr| format!("  {}", instr_to_str(&instr)))
        .collect::<Vec<String>>()
        .join("\n");

    let serialized = serialize_struct_layouts(&struct_sigs, &struct_layouts);

    let full_program = format!(
        "
section .data
global structHashmap
structHashmap db \"{serialized}\", 0

section .bss
global {BUFFER_NAME}
{BUFFER_NAME}: resb {BUFFER_SIZE} ; reserve {BUFFER_SIZE} bytes for array

section .text

extern snek_print
extern snek_error

global our_code_starts_here
{instrs_string}

"
    );
    full_program
}
