use std::vec;

use im::HashMap;

use crate::compile_shared::{
    compute_aligned_rsp_offset, emit_program, generate_label, increment_counter, instr_to_string,
    type_to_flag,
};
use crate::consts::*;
use crate::semantics::{struct_name_to_type_enum, struct_type_enum_to_name, STRUCT_NUM_TO_NAME};
use crate::structs::*;

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
    let (label_true, label_finish) = match op {
        Op2::Greater | Op2::GreaterEqual | Op2::Equal | Op2::LessEqual | Op2::Less => {
            let label_true = generate_label(label_name, increment_counter(label_counter));
            let label_finish = generate_label(label_name, increment_counter(label_counter));
            (Some(label_true), Some(label_finish))
        }

        _ => (None, None),
    };

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
        Op2::Or | Op2::And => {
            instr_to_compute_res.push(match op {
                Op2::Or => Instr::LOR,
                Op2::And => Instr::LAND,
                _ => panic!("Unexpected: This should never be called"),
            }(
                Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, a_rsp_offset)
            ));
        }
        _ => {
            instr_to_compute_res.push(Instr::Compare(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            ));

            let label_true = label_true.unwrap();
            let label_finish = label_finish.unwrap();

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
    let mut intrs = match e {
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
                Op1::Add1 => {
                    instructions.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));

                    // Do Overflow Checking
                    instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
                    instructions.push(Instr::JumpOverflow(String::from(ERROR_LABEL)));
                }
                Op1::Sub1 => {
                    instructions.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));

                    // Do Overflow Checking
                    instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
                    instructions.push(Instr::JumpOverflow(String::from(ERROR_LABEL)));
                }
                Op1::Not => instructions.push(Instr::LXOR(Val::Reg(Reg::RAX), Val::Imm(1))),
                Op1::Print => {
                    let flag = type_to_flag(t);

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

        TypedExpr::If(_, cond, if_true, if_false) => {
            let mut instr_to_compile_if: Vec<Instr> = vec![];
            let left_label = generate_label(label_name, increment_counter(label_counter));
            let if_finish_label = generate_label(label_name, increment_counter(label_counter));

            // Conditional Expression
            let code_to_eval_expr = compile_expr_to_instrs(
                cond,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            );
            instr_to_compile_if.extend(code_to_eval_expr);
            instr_to_compile_if.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(1)));

            instr_to_compile_if.push(Instr::JumpEqual(left_label.clone()));

            // Right Branch
            let code_to_eval_expr2 = compile_expr_to_instrs(
                if_false,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            );
            instr_to_compile_if.extend(code_to_eval_expr2);

            instr_to_compile_if.push(Instr::Jump(if_finish_label.clone()));

            // Left Branch
            instr_to_compile_if.push(Instr::AddLabel(left_label.clone()));
            let code_to_eval_expr1 = compile_expr_to_instrs(
                if_true,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            );
            instr_to_compile_if.extend(code_to_eval_expr1);

            // Finish
            instr_to_compile_if.push(Instr::AddLabel(if_finish_label.clone()));

            instr_to_compile_if
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

        TypedExpr::Call(_ret_type, fun_name_or_ptr, args) => {
            /*
            foo(a, b) =>
            |    a        | <-- RSP + 16
            |    b        | <-- RSP + 8
            | return addr | <-- RSP
            */

            let (fun_label, is_in_rax) = match **fun_name_or_ptr {
                // immediate names go as strings
                TypedExpr::FunName(_, ref fun_name) => {
                    (FunctionLabel::Custom(fun_name.to_owned()), false)
                }

                // pointers should be compiled into rax
                ref texpr => {
                    if let ExprType::FunctionPointer(_, _) = extract_type(texpr) {
                        (FunctionLabel::Pointer(Reg::RAX), true)
                    } else {
                        unreachable!("already typechecks")
                    }
                }
            };

            let mut inst_to_call = Vec::new();
            let mut curr_rsp_offset = rsp_offset;

            let ptr_rsp_offset = if is_in_rax {
                // compile function pointer
                inst_to_call.extend(compile_expr_to_instrs(
                    &fun_name_or_ptr,
                    scope_bindings.clone(),
                    struct_layouts,
                    curr_rsp_offset,
                    label_counter,
                    label_name,
                ));

                // push pointer onto stack
                curr_rsp_offset -= SIZEOF_I_64;
                inst_to_call.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, curr_rsp_offset),
                    Val::Reg(Reg::RAX),
                ));

                curr_rsp_offset
            } else {
                -1
            };

            // what rsp would be after pushing args
            let unadjusted_call_rsp = curr_rsp_offset - (SIZEOF_I_64 * args.len() as i32);

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
                inst_to_call.extend(instr_to_eval_arg);

                // store this argument on the stack
                curr_rsp_offset -= SIZEOF_I_64;
                inst_to_call.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, curr_rsp_offset),
                    Val::Reg(Reg::RAX),
                ));
            }

            if is_in_rax {
                // move function pointer address, stored on the stack, into RAX
                inst_to_call.push(Instr::IMov(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, ptr_rsp_offset),
                ));

                inst_to_call.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(0))); // if val is null
                inst_to_call.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                inst_to_call.push(Instr::JumpEqual(ERROR_LABEL.to_string())); // jump to ERROR_LABEL
            }

            // update RSP for the function call
            inst_to_call.push(Instr::IAdd(
                Val::Reg(Reg::RSP),
                Val::Imm(compute_aligned_rsp_offset(curr_rsp_offset)),
            )); // Reset Alignment

            // call the function
            inst_to_call.push(Instr::Call(fun_label));

            inst_to_call.push(Instr::ISub(
                Val::Reg(Reg::RSP),
                Val::Imm(compute_aligned_rsp_offset(curr_rsp_offset)),
            )); // Reset Alignment

            inst_to_call
        }

        TypedExpr::Null(_) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0))],

        TypedExpr::Alloc(expr_type) => {
            let mut instructions = Vec::new();
            let debug: &'static str = "create";
            let type_string = match expr_type {
                ExprType::Bool => {
                    panic!(
                        "Unexpected: Attempted to {debug} a Bool pointer. This should not happen"
                    )
                }
                ExprType::Int => {
                    panic!(
                        "Unexpected: Attempted to {debug} an Int pointer. This should not happen"
                    )
                }
                ExprType::Unit => panic!(
                    "Unexpected: Attempted to {debug} an Unit pointer. This should not happen"
                ),
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
                ExprType::FunctionPointer(_arg_types, _ret_type) => {
                    panic!("Unexpected: Attempted to {debug} an Function pointer pointer. This should not happen")
                }
                ExprType::Array(_expr_type) => todo!(),
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

        TypedExpr::Update(_, ptr, field_name, new_val) => {
            let mut instructions = Vec::new();
            let debug: &'static str = "access";
            let expr_type = extract_type(ptr);
            let type_string = match expr_type {
                ExprType::Bool => {
                    panic!("Unexpected: Attempted to {debug} a Bool pointer. This should not happen")
                }
                ExprType::Int => {
                    panic!("Unexpected: Attempted to {debug} an Int pointer. This should not happen")
                }
                ExprType::Unit => panic!(
                    "Unexpected: Attempted to {debug} an Unit pointer. This should not happen"
                ),
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
                ExprType::FunctionPointer(_arg_types, _ret_type) =>
                    panic!("Unexpected: Attempted to {debug} a Function pointer pointer. This should not happen"),
                ExprType::Array(_expr_type) => todo!(),
            };
            let offset = match struct_layouts.get(&type_string) {
                Some(StructLayout::Layout(layout_dict)) => match layout_dict.get(field_name) {
                    Some(i) => i,
                    None => panic!("Unexpected: Missing field. This should not happen"),
                },
                None => panic!("Unexpected: Broken structure dictionary. This should not happen"),
            };

            let instr_to_get_val = compile_expr_to_instrs(
                new_val,
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
                ptr,
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
            )); // RDI now holds new value of field

            instructions.push(Instr::IMov(
                Val::RegOffset(Reg::RAX, SIZEOF_I_64 * offset),
                Val::Reg(Reg::RDI),
            ));

            instructions.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));

            instructions
        }

        TypedExpr::Lookup(_, typed_expr, field_name) => {
            let mut instructions = Vec::new();
            let debug: &'static str = "access";
            let expr_type = extract_type(typed_expr);
            let type_string = match expr_type {
                ExprType::Bool => {
                    panic!("Unexpected: Attempted to {debug} a Bool pointer. This should not happen")
                }
                ExprType::Int => {
                    panic!("Unexpected: Attempted to {debug} an Int pointer. This should not happen")
                }
                ExprType::Unit => panic!(
                    "Unexpected: Attempted to {debug} an Unit pointer. This should not happen"
                ),
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
                ExprType::FunctionPointer(_arg_types, _ret_type) =>
                    panic!("Unexpected: Attempted to {debug} a Function pointer pointer. This should not happen"),
                ExprType::Array(_expr_type) => todo!(),
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
        TypedExpr::Unit => vec![],
        TypedExpr::FunName(_expr_type, name) => {
            vec![Instr::Lea(Val::Reg(Reg::RAX), Val::Global(name.to_owned()))]
        }
        TypedExpr::ArrayAlloc(_elem_t, len) => {
            let mut instructions = compile_expr_to_instrs(
                len,
                scope_bindings.clone(),
                &struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            ); // requested num elems is in RAX

            // Throw runtime if num of elements is <= 0
            instructions.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(1))); // num_elem ? 1
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(4))); // num_elem < 1 : error
            instructions.push(Instr::JumpLess(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            // Put num elements on top of stack
            let mut curr_rsp_offset = rsp_offset;
            curr_rsp_offset -= SIZEOF_I_64; // get 8 bytes of space on the stack
            let a_rsp_offset = curr_rsp_offset;
            instructions.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, a_rsp_offset),
                Val::Reg(Reg::RAX),
            ));

            // Convert from number of elements to size, alloc size should be in RAX
            instructions.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
            instructions.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(SIZEOF_I_64)));
            // size (byte) is in RAX

            // logic: if (RBX + alloc_size) > BUFFER_SIZE, go to ERROR_LABEL
            instructions.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // rax := (num bytes allocated + num bytes needed)
            instructions.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(BUFFER_SIZE))); // if (num bytes allocated + num bytes needed) > BUFFER_SIZE:
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2)));
            instructions.push(Instr::JumpGreater(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            // Put unaltered size-- number of elements-- in RDI (will write to 0th elem of backing array)
            instructions.push(Instr::IMov(
                Val::Reg(Reg::RDI),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            ));

            // we have enough room; load in array to rax
            instructions.push(Instr::Lea(
                Val::Reg(Reg::RAX),
                Val::Global(BUFFER_NAME.to_string()),
            )); // rax := &array
            instructions.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // rax := &array + rbx

            instructions.push(Instr::IMov(Val::RegOffset(Reg::RAX, 0), Val::Reg(Reg::RDI))); // mov (rax), RDI

            instructions.push(Instr::IAdd(Val::Reg(Reg::RDI), Val::Imm(1)));
            instructions.push(Instr::IMul(Val::Reg(Reg::RDI), Val::Imm(SIZEOF_I_64)));

            // update the total bytes allocated, so that the next alloc points to next free addrr
            instructions.push(Instr::IAdd(Val::Reg(Reg::RBX), Val::Reg(Reg::RDI))); // rbx := num bytes allocated + num bytes needed
            instructions
        }

        TypedExpr::ArrayLen(_elem_t, arr_addr) => {
            let mut instructions = compile_expr_to_instrs(
                arr_addr,
                scope_bindings,
                struct_layouts,
                rsp_offset,
                label_counter,
                label_name,
            );

            instructions.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(0))); // if arr is null
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
            instructions.push(Instr::JumpEqual(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            // didn't die, so struct ptr isn't null, so access
            instructions.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RAX, SIZEOF_I_64 * 0),
            ));

            instructions
        }
        TypedExpr::ArrayLookup(_elem_t, arr, ind_expr) => {
            let mut curr_rsp_offset = rsp_offset;
            let mut instructions = Vec::new();

            instructions.extend(compile_expr_to_instrs(
                arr,
                scope_bindings.clone(),
                &struct_layouts,
                curr_rsp_offset,
                label_counter,
                label_name,
            )); // requested arr is in RAX

            // if arr ptr is null, die
            instructions.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(0))); // if val is null
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
            instructions.push(Instr::JumpEqual(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            // If not null, put arr on top of stack
            curr_rsp_offset -= SIZEOF_I_64; // get 8 bytes of space on the stack
            let a_rsp_offset = curr_rsp_offset;
            instructions.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, a_rsp_offset),
                Val::Reg(Reg::RAX),
            ));

            instructions.extend(compile_expr_to_instrs(
                ind_expr,
                scope_bindings.clone(),
                &struct_layouts,
                curr_rsp_offset,
                label_counter,
                label_name,
            )); // index now in rax

            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))); // RDI := index
            instructions.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            )); // RAX := address of array
            instructions.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, a_rsp_offset),
                Val::Reg(Reg::RDI),
            )); // index on stack

            // didn't die, so arr ptr isn't null, so test bounds
            instructions.push(Instr::Compare(Val::Reg(Reg::RDI), Val::Imm(0))); // if indice is negative fail
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(5)));
            instructions.push(Instr::JumpLess(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            instructions.push(Instr::IMov(
                Val::Reg(Reg::RDI),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            )); // rdi now contains requested index
            instructions.push(Instr::Compare(
                Val::Reg(Reg::RDI),
                Val::RegOffset(Reg::RAX, 0),
            )); // if indice is greater or equal to len fail
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(5)));
            instructions.push(Instr::JumpGreaterEqual(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            // null & bounds check cleared; update element
            instructions.push(Instr::IMov(
                Val::Reg(Reg::RDI),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            )); // rdi now contains requested elem index
            instructions.push(Instr::IAdd(Val::Reg(Reg::RDI), Val::Imm(1)));
            instructions.push(Instr::IMul(Val::Reg(Reg::RDI), Val::Imm(SIZEOF_I_64))); // rdi now contains index in bytes

            instructions.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))); // rax now contains address of array index to write to
            instructions.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RAX, SIZEOF_I_64 * 0),
            )); // rax now contains element

            instructions
        }
        TypedExpr::ArrayUpdate(_elem_t, arr, ind_expr, new_val) => {
            let mut curr_rsp_offset = rsp_offset;
            let mut instructions = Vec::new();

            instructions.extend(compile_expr_to_instrs(
                arr,
                scope_bindings.clone(),
                &struct_layouts,
                curr_rsp_offset,
                label_counter,
                label_name,
            )); // requested arr is in RAX

            // if arr ptr is null, die
            instructions.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(0))); // if val is null
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
            instructions.push(Instr::JumpEqual(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            // If not null, put arr on top of stack
            curr_rsp_offset -= SIZEOF_I_64; // get 8 bytes of space on the stack
            let a_rsp_offset = curr_rsp_offset;
            instructions.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, a_rsp_offset),
                Val::Reg(Reg::RAX),
            ));

            instructions.extend(compile_expr_to_instrs(
                ind_expr,
                scope_bindings.clone(),
                &struct_layouts,
                curr_rsp_offset,
                label_counter,
                label_name,
            )); // index now in rax

            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))); // RDI := index
            instructions.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            )); // RAX := address of array
            instructions.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, a_rsp_offset),
                Val::Reg(Reg::RDI),
            )); // index on stack

            // didn't die, so arr ptr isn't null, so test bounds
            instructions.push(Instr::Compare(Val::Reg(Reg::RDI), Val::Imm(0))); // if indice is negative fail
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(5)));
            instructions.push(Instr::JumpLess(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            instructions.push(Instr::IMov(
                Val::Reg(Reg::RDI),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            )); // rdi now contains requested index
            instructions.push(Instr::Compare(
                Val::Reg(Reg::RDI),
                Val::RegOffset(Reg::RAX, 0),
            )); // if indice is greater or equal to len fail
            instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(5)));
            instructions.push(Instr::JumpGreaterEqual(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

            // null & bounds check cleared; get address of element
            instructions.push(Instr::IMov(
                Val::Reg(Reg::RDI),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            )); // rdi now contains requested elem index
            instructions.push(Instr::IAdd(Val::Reg(Reg::RDI), Val::Imm(1)));
            instructions.push(Instr::IMul(Val::Reg(Reg::RDI), Val::Imm(SIZEOF_I_64))); // rdi now contains index in bytes
            instructions.push(Instr::IAdd(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))); // rdi now contains address of array index to write to

            instructions.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, a_rsp_offset),
                Val::Reg(Reg::RDI),
            ));

            instructions.extend(compile_expr_to_instrs(
                new_val,
                scope_bindings,
                struct_layouts,
                curr_rsp_offset,
                label_counter,
                label_name,
            )); // RAX now contains new value

            instructions.push(Instr::IMov(
                Val::Reg(Reg::RDI),
                Val::RegOffset(Reg::RSP, a_rsp_offset),
            )); // rdi now contains address of array index to

            instructions.push(Instr::IMov(Val::RegOffset(Reg::RDI, 0), Val::Reg(Reg::RAX)));

            instructions
        }
    };

    intrs.push(Instr::NoOp(
        match e {
            TypedExpr::Number(_) => "Number",
            TypedExpr::Boolean(_) => "Boolean",
            TypedExpr::Id(_, _) => "Id",
            TypedExpr::Let(_, _, _) => "Let",
            TypedExpr::UnOp(_, _, _) => "UnOp",
            TypedExpr::BinOp(_, _, _, _) => "BinOp",
            TypedExpr::If(_, _, _, _) => "If",
            TypedExpr::RepeatUntil(_, _, _) => "RepeatUntil",
            TypedExpr::Set(_, _, _) => "Set",
            TypedExpr::Block(_, _) => "Block",
            TypedExpr::FunName(_, _) => "FunName",
            TypedExpr::Call(_, _, _) => "Call",
            TypedExpr::Input => "Input",
            TypedExpr::RDInput => "RDInput",
            TypedExpr::Null(_) => "Null",
            TypedExpr::Alloc(_) => "Alloc",
            TypedExpr::Update(_, _, _, _) => "Update",
            TypedExpr::Lookup(_, _, _) => "Lookup",
            TypedExpr::Unit => "Unit",
            TypedExpr::ArrayAlloc(_, _) => "ArrayAlloc",
            TypedExpr::ArrayLookup(_, _, _) => "ArrayLookup",
            TypedExpr::ArrayUpdate(_, _, _, _) => "ArrayUpdate",
            TypedExpr::ArrayLen(_, _) => "ArrayLen",
        }
        .to_string(),
    ));
    intrs
}

fn compile_fn(f: &TypedFunction, struct_layouts: &HashMap<String, StructLayout>) -> Vec<Instr> {
    let TypedFunction::Fun(fun_name, FunSignature::Sig(_ret_type, args), typed_body) = f;

    // add the label for our code
    let mut instrs = vec![
        Instr::Align(8), // to use as function pointers
        Instr::AddLabel(fun_name.to_string()),
    ];

    if fun_name == ENTRYPOINT_LABEL {
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
                    ExprType::Unit => "unit".to_string(),
                    ExprType::StructPointer(struct_type_enum) => {
                        match struct_type_enum_to_name(*struct_type_enum) {
                            Some(s) => s,
                            None => unreachable!(),
                        }
                    }
                    ExprType::FunctionPointer(_arg_type, _ret_type) => {
                        "todo!(function pointer)".to_string()
                    }
                    ExprType::Array(_expr_type) => "todo!(array)".to_string(),
                },
                None => unreachable!(),
            });
        }
        res_vec.push(subres_vec);
    }

    let res_vec: Vec<String> = res_vec.into_iter().map(|subres| subres.join(".")).collect();
    res_vec.join(",")
}

pub fn compile_prog(tp: &TypedProg) -> Vec<Instr> {
    let TypedProg::Program(body_type, struct_sigs, struct_layouts, typed_funs, typed_e) = tp;

    let mut all_instrs = Vec::new();

    for typed_fun in typed_funs {
        all_instrs.extend(compile_fn(typed_fun, struct_layouts));
    }

    // compile program body, with the label __main
    let main = TypedFunction::Fun(
        MAIN_LABEL.to_string(),
        FunSignature::Sig(
            body_type.clone(),
            vec![(ExprType::Int, "input".to_string())],
        ), // input: int -> body_type
        typed_e.clone(),
    );
    all_instrs.extend(compile_fn(&main, &struct_layouts));

    // compile the entrypoint, which loads RDI (input) onto the stack, then calls __main
    let entrypoint = TypedFunction::Fun(
        // (print (main input : int ) : body_type ) : body_type
        ENTRYPOINT_LABEL.to_string(),
        FunSignature::Sig(body_type.clone(), Vec::new()), // unit -> body_type
        TypedExpr::UnOp(
            body_type.clone(),
            Op1::Print,
            Box::new(TypedExpr::Call(
                body_type.clone(),
                Box::new(TypedExpr::FunName(
                    body_type.clone(), // TODO
                    MAIN_LABEL.to_string(),
                )),
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

    all_instrs
}

#[derive(PartialEq, Debug)]
pub struct StructSerializer {
    pub struct_sigs: HashMap<String, StructSignature>, // to know the types of fields
    pub struct_layouts: HashMap<String, StructLayout>, // to know the offsets of fields
}

impl std::fmt::Display for StructSerializer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            serialize_struct_layouts(&self.struct_sigs, &self.struct_layouts)
        )
    }
}