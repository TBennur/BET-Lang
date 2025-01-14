use crate::alt_stack::*;
use crate::compile_shared::*;
use crate::consts::*;

use crate::semantics::STRUCT_NUM_TO_NAME;
use crate::structs::*;
use crate::typecheck_fast_shared::TypeCheckState;


/* --- Helper Functions --- */

pub fn fast_type_to_flag(t: FastExprType, type_set: &TypeCheckState) -> i32 {
    // TODO: consider just returning FastExprType?
    type_to_flag(t.to_expr_type_ref(type_set))
}

struct NewStackStatePlus<From, To, C>
where
    C: ConstructorPlus<To>,
{
    unparsed: Vec<From>,
    parsed: Vec<To>,
    len: usize,
    constructor: C,
}

impl<From, To, C> NewStackStatePlus<From, To, C>
where
    C: ConstructorPlus<To>,
{
    pub fn new(mut unparsed: Vec<From>, constructor: C) -> Self {
        let len = unparsed.len();

        if len == 0 {
            panic!("Zero parsing to do!")
        }

        unparsed.reverse(); // will pop from back
        NewStackStatePlus {
            unparsed,
            parsed: Vec::with_capacity(len),
            len,
            constructor,
        }
    }
}

type LocalCompileState<'a, 'b> =
    NewStackStatePlus<WrappedFTE<'a, 'b>, Vec<Instr>, InstrsConstructor<'a>>;

struct GlobalCompileState<'a, 'b> {
    type_map: &'b TypeCheckState,
    label_map: std::collections::HashMap<&'a str, i64>,
}

impl<'a, 'b> GlobalCompileState<'a, 'b> {
    fn increment_counter(&mut self, label_name: &'a str) -> i64 {
        let new_val = match self.label_map.get(label_name) {
            Some(x) => *x,
            None => 0,
        } + 1;
        self.label_map.insert(label_name, new_val);
        new_val
    }
}

struct WrappedFTE<'a: 'b, 'b> {
    fte: &'b FastTypedExpr<'a>,
    scope_bindings: im::HashMap<&'a str, i32>,
    struct_layouts: &'b im::HashMap<&'a str, FastStructLayout<'a>>,
    rsp_offset: i32,
    label_counter: (),
    label_name: &'a str,
}

enum UnOpConstructor {
    Print { flag: i32, rsp_offset: i32 }, // type flag
    Other(Op1),
}

enum InstrsConstructor<'a> {
    UnOp(UnOpConstructor),
    Let {
        id_rsp_offsets: Vec<i32>,
    },
    If {
        label_name: &'a str,
        left_label: i64,
        if_finish_label: i64,
    },
    Set(i32),
    Block,
    RepeatUntil {
        label_name: &'a str,
        body_label: i64,
        id_rsp_offset: i32,
    },
    Call {
        fun_label: FunctionLabel,
        is_in_rax: bool,
        rsp_offset: i32,
    },
    Update {
        offset: i32,
        curr_rsp_offset: i32,
    },
    Lookup {
        offset: i32,
    },
    ArrayAlloc {
        rsp_offset: i32,
    },
    ArrayLen,
    ArrayLookup {
        rsp_offset: i32,
    },
    ArrayUpdate {
        rsp_offset: i32,
    },
    BinOp {
        rsp_offset: i32,
        op: Op2,
        label_name: &'a str,
        label_true: i64,
        label_finish: i64,
    },
}

impl<'a> ConstructorPlus<Vec<Instr>> for InstrsConstructor<'a> {
    type State<'b> = GlobalCompileState<'a, 'b>;

    fn construct<'b>(
        &mut self,
        instructions: &mut Vec<Vec<Instr>>,
        _state: &mut GlobalCompileState<'a, 'b>,
    ) -> Vec<Instr> {
        let mut instrs = match self {
            InstrsConstructor::UnOp(un_op_constructor) => {
                let [mut instructions] = std::mem::take(instructions).try_into().unwrap();

                match un_op_constructor {
                    UnOpConstructor::Other(Op1::Add1) => {
                        instructions.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));

                        // Do Overflow Checking
                        instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
                        instructions.push(Instr::JumpOverflow(String::from(ERROR_LABEL)));
                    }
                    UnOpConstructor::Other(Op1::Sub1) => {
                        instructions.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));

                        // Do Overflow Checking
                        instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
                        instructions.push(Instr::JumpOverflow(String::from(ERROR_LABEL)));
                    }
                    UnOpConstructor::Other(Op1::Not) => {
                        instructions.push(Instr::LXOR(Val::Reg(Reg::RAX), Val::Imm(1)))
                    }
                    UnOpConstructor::Print { flag, rsp_offset } => {
                        instructions.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(*flag)));
                        instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))); // load val into rdi

                        instructions.push(Instr::IAdd(
                            Val::Reg(Reg::RSP),
                            Val::Imm(compute_aligned_rsp_offset(*rsp_offset)),
                        )); // Reset Alignment
                        instructions.push(Instr::Lea(
                            Val::Reg(Reg::RDX),
                            Val::Global("structHashmap".to_string()),
                        ));
                        instructions.push(Instr::Call(FunctionLabel::SnekPrint));

                        instructions.push(Instr::ISub(
                            Val::Reg(Reg::RSP),
                            Val::Imm(compute_aligned_rsp_offset(*rsp_offset)),
                        )); // Reset Alignment
                    }
                    UnOpConstructor::Other(Op1::Print) => unreachable!(),
                }
                instructions
            }

            InstrsConstructor::BinOp {
                rsp_offset,
                op,
                label_name,
                label_true,
                label_finish,
            } => {
                let mut rsp_offset = *rsp_offset;
                let label_name = *label_name;
                let label_true = *label_true;
                let label_finish = *label_finish;

                let [a_insts, b_insts] = std::mem::take(instructions).try_into().unwrap();

                let mut instructions = a_insts;

                // store that value on the stack
                rsp_offset -= SIZEOF_I_64; // get 4 bytes of space on the stack
                let a_rsp_offset = rsp_offset;
                instructions.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, a_rsp_offset),
                    Val::Reg(Reg::RAX),
                ));

                // this computes b, and stores it in RAX. since we adjusted rsp_offset,
                // our stored value of a is still at a_rsp_offset
                instructions.extend(b_insts);

                // now we have `a` at the memory location RSP + a_rsp_offset, and `b` in RAX
                // use OP to compute OP(b <rax>, a <rsp + a_rsp_offset>)

                match op {
                    Op2::Minus | Op2::Plus | Op2::Times => {
                        instructions.push(match op {
                            Op2::Minus => Instr::ISub,
                            Op2::Plus => Instr::IAdd,
                            Op2::Times => Instr::IMul,
                            _ => panic!("Unexpected: This should never be called"),
                        }(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, a_rsp_offset),
                        ));
                        // Do Overflow Checking
                        instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
                        instructions.push(Instr::JumpOverflow(String::from(ERROR_LABEL)));
                    }
                    Op2::Or | Op2::And => {
                        instructions.push(match op {
                            Op2::Or => Instr::LOR,
                            Op2::And => Instr::LAND,
                            _ => panic!("Unexpected: This should never be called"),
                        }(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, a_rsp_offset),
                        ));
                    }
                    _ => {
                        instructions.push(Instr::Compare(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, a_rsp_offset),
                        ));

                        let label_true = generate_label(label_name, label_true);
                        let label_finish = generate_label(label_name, label_finish);

                        instructions.push(match op {
                            Op2::Greater => Instr::JumpGreater,
                            Op2::GreaterEqual => Instr::JumpGreaterEqual,
                            Op2::Equal => Instr::JumpEqual,
                            Op2::LessEqual => Instr::JumpLessEqual,
                            Op2::Less => Instr::JumpLess,
                            _ => panic!("Unexpected: This should never be called"),
                        }(label_true.clone()));
                        instructions.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));
                        instructions.push(Instr::Jump(label_finish.clone()));
                        instructions.push(Instr::AddLabel(label_true.clone()));
                        instructions.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                        instructions.push(Instr::Jump(label_finish.clone()));
                        instructions.push(Instr::AddLabel(label_finish.clone()));
                    }
                };

                instructions
            }
            InstrsConstructor::Let { id_rsp_offsets } => {
                let num_bindings = id_rsp_offsets.len();
                assert!(num_bindings + 1 == instructions.len()); // bindings, plus let body

                let existing_instrs_len = instructions
                    .iter()
                    .fold(0usize, |acc, inner| acc + inner.len());
                let all_instrs_len = existing_instrs_len + num_bindings; // one additional instruction per binding: moving RAX to RSP + ID_OFFSET
                let mut all_instrs = Vec::with_capacity(all_instrs_len);

                let let_body = instructions.pop().unwrap();
                // instructions is now a nested Vec of the instructions needed to compile each binding

                let instructions = std::mem::take(instructions);
                for (instrs, id_rsp_offset) in instructions.into_iter().zip(id_rsp_offsets) {
                    all_instrs.extend(instrs);
                    all_instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RSP, *id_rsp_offset),
                        Val::Reg(Reg::RAX),
                    ));
                }
                all_instrs.extend(let_body);
                all_instrs
            }
            InstrsConstructor::If {
                label_name,
                left_label,
                if_finish_label,
            } => {
                let label_name: &'a str = &label_name;
                let left_label = *left_label;
                let if_finish_label = *if_finish_label;

                let [cond, if_false, if_true] = std::mem::take(instructions).try_into().unwrap();

                // Conditional Expression
                let mut instr_to_compile_if = cond;

                instr_to_compile_if.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(1)));
                let left_label = generate_label(label_name, left_label);
                instr_to_compile_if.push(Instr::JumpEqual(left_label.clone()));

                // Right Branch
                instr_to_compile_if.extend(if_false);

                let if_finish_label = generate_label(label_name, if_finish_label);
                instr_to_compile_if.push(Instr::Jump(if_finish_label.clone()));

                // Left Branch
                instr_to_compile_if.push(Instr::AddLabel(left_label.clone()));
                instr_to_compile_if.extend(if_true);

                // Finish
                instr_to_compile_if.push(Instr::AddLabel(if_finish_label.clone()));

                instr_to_compile_if
            }
            InstrsConstructor::Set(id_rsp_offset) => {
                // get the asm instructions to evaluate `expr` into rax
                let [mut instr_to_compile_set] = std::mem::take(instructions).try_into().unwrap();

                // get the rsp offset where this variable is stored
                let id_rsp_offset = *id_rsp_offset;

                // add instruction to update value of binding
                instr_to_compile_set.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, id_rsp_offset),
                    Val::Reg(Reg::RAX),
                ));

                // don't change the value in rax since the set expression evaluates to the new value
                instr_to_compile_set
            }
            InstrsConstructor::Block => {
                let instructions = std::mem::take(instructions);
                instructions.into_iter().flatten().collect() // value of last expression in block is already in rax, so done
            }
            InstrsConstructor::RepeatUntil {
                label_name,
                body_label,
                id_rsp_offset,
            } => {
                let label_name = *label_name;
                let body_label = *body_label;

                let id_rsp_offset = *id_rsp_offset;

                let [body, stop_cond] = std::mem::take(instructions).try_into().unwrap();
                let mut instructions_to_compile_repeat_until = vec![];

                // add body label
                let body_label = generate_label(label_name, body_label);
                instructions_to_compile_repeat_until.push(Instr::AddLabel(body_label.clone()));

                // compile the body
                instructions_to_compile_repeat_until.extend(body);

                // push the value of body (rax) onto stack
                instructions_to_compile_repeat_until.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, id_rsp_offset),
                    Val::Reg(Reg::RAX),
                ));

                // evaluate the stop condition, which moves its result into rax
                instructions_to_compile_repeat_until.extend(stop_cond);

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
            InstrsConstructor::Call {
                fun_label,
                is_in_rax,
                rsp_offset,
            } => {
                let fun_label = std::mem::replace(fun_label, FunctionLabel::SnekError);
                let is_in_rax = *is_in_rax;
                let rsp_offset = *rsp_offset;

                let mut inst_to_call: Vec<Instr> = Vec::new();
                let mut curr_rsp_offset = rsp_offset;

                let ptr_rsp_offset = if is_in_rax {
                    // compiled function pointer
                    inst_to_call.extend(instructions.remove(0)); // TODO POP FRONT

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

                // we popped the function pointer if it was compiled, so now all we have left are args
                let compiled_args = std::mem::take(instructions);

                // what rsp would be after pushing args
                let unadjusted_call_rsp =
                    curr_rsp_offset - (SIZEOF_I_64 * compiled_args.len() as i32);

                // preemptively adjust rsp so that it will 8 byte aligned, mod 16, after pushing args
                curr_rsp_offset +=
                    compute_aligned_rsp_offset(unadjusted_call_rsp) - unadjusted_call_rsp;

                // evaluate the arguments, pushing each onto the stack
                for instr_to_eval_arg in compiled_args {
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
                    inst_to_call.push(Instr::JumpEqual(ERROR_LABEL.to_string()));
                    // jump to ERROR_LABEL
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
            InstrsConstructor::Update {
                offset,
                curr_rsp_offset,
            } => {
                let offset = *offset;
                let curr_rsp_offset = *curr_rsp_offset;

                let [new_val, ptr] = std::mem::take(instructions).try_into().unwrap();

                let mut instructions = new_val;
                instructions.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, curr_rsp_offset),
                    Val::Reg(Reg::RAX),
                ));

                instructions.extend(ptr);

                instructions.push(Instr::Compare(Val::Reg(Reg::RAX), Val::Imm(0))); // if val is null
                instructions.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(3)));
                instructions.push(Instr::JumpEqual(ERROR_LABEL.to_string())); // jump to ERROR_LABEL

                instructions.push(Instr::IMov(
                    Val::Reg(Reg::RDI),
                    Val::RegOffset(Reg::RSP, curr_rsp_offset),
                )); // RDI now holds new value of field

                instructions.push(Instr::IMov(
                    Val::RegOffset(Reg::RAX, SIZEOF_I_64 * offset),
                    Val::Reg(Reg::RDI),
                ));

                instructions.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));

                instructions
            }
            InstrsConstructor::Lookup { offset } => {
                let offset = *offset;
                let [mut instructions] = std::mem::take(instructions).try_into().unwrap();

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
            InstrsConstructor::ArrayAlloc { rsp_offset } => {
                let rsp_offset = *rsp_offset;

                // requested num elems is in RAX
                let [mut instructions] = std::mem::take(instructions).try_into().unwrap();

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
            InstrsConstructor::ArrayLen => {
                let [mut instructions] = std::mem::take(instructions).try_into().unwrap();

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
            InstrsConstructor::ArrayLookup { rsp_offset } => {
                let rsp_offset = *rsp_offset;
                let mut curr_rsp_offset = rsp_offset;

                let [arr_comp, ind_comp] = std::mem::take(instructions).try_into().unwrap();

                let mut instructions = arr_comp; // requested arr is in RAX

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

                instructions.extend(ind_comp); // index now in rax

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
            InstrsConstructor::ArrayUpdate { rsp_offset } => {
                let rsp_offset = *rsp_offset;
                let mut curr_rsp_offset = rsp_offset;

                let [arr_instrs, ind_instrs, new_val_instrs] =
                    std::mem::take(instructions).try_into().unwrap();

                let mut instructions = arr_instrs;

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

                instructions.extend(ind_instrs); // index now in rax

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

                instructions.extend(new_val_instrs); // RAX now contains new value

                instructions.push(Instr::IMov(
                    Val::Reg(Reg::RDI),
                    Val::RegOffset(Reg::RSP, a_rsp_offset),
                )); // rdi now contains address of array index to

                instructions.push(Instr::IMov(Val::RegOffset(Reg::RDI, 0), Val::Reg(Reg::RAX)));

                instructions
            }
        };

        instrs.push(Instr::NoOp(
            match self {
                InstrsConstructor::UnOp(_) => "UnOp",
                InstrsConstructor::Let { .. } => "Let",
                InstrsConstructor::If { .. } => "If",
                InstrsConstructor::Set(_) => "Set",
                InstrsConstructor::Block => "Block",
                InstrsConstructor::RepeatUntil { .. } => "RepeatUntil",
                InstrsConstructor::Call { .. } => "Call",
                InstrsConstructor::Update { .. } => "Update",
                InstrsConstructor::Lookup { .. } => "Lookup",
                InstrsConstructor::ArrayAlloc { .. } => "ArrayAlloc",
                InstrsConstructor::ArrayLen => "ArrayLen",
                InstrsConstructor::ArrayLookup { .. } => "ArrayLookup",
                InstrsConstructor::ArrayUpdate { .. } => "ArrayUpdate",
                InstrsConstructor::BinOp { .. } => "BinOp",
            }
            .to_string(),
        ));
        instrs
    }
}

const ILLEGAL: &'static FastTypedExpr = &FastTypedExpr::__INTERNAL;

impl<'a: 'b, 'b> crate::alt_stack::OneStep<GlobalCompileState<'a, 'b>, LocalCompileState<'a, 'b>>
    for WrappedFTE<'a, 'b>
{
    fn step(
        mut self,
        state: &mut GlobalCompileState<'a, 'b>,
    ) -> StepResult<Vec<Instr>, LocalCompileState<'a, 'b>> {
        let cur = self.fte;
        self.fte = ILLEGAL;
        match cur {
            FastTypedExpr::__INTERNAL => unreachable!(),

            FastTypedExpr::RDInput => StepResult::Terminal(vec![
                Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)),
                Instr::NoOp("RDInput".to_string()),
            ]),

            // immediate values
            FastTypedExpr::Boolean(b) => StepResult::Terminal(match b {
                false => vec![
                    Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)),
                    Instr::NoOp("Boolean".to_string()),
                ],
                true => vec![
                    Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)),
                    Instr::NoOp("Boolean".to_string()),
                ],
            }),
            FastTypedExpr::Number(x) => StepResult::Terminal(vec![
                Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*x)),
                Instr::NoOp("Number".to_string()),
            ]),

            // input will be passed to main as any other argument: on the stack
            FastTypedExpr::Input => match self.scope_bindings.get("input") {
                None => panic!("Unbound variable identifier {:?}", "input"),
                Some(offset) => StepResult::Terminal(vec![
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *offset)),
                    Instr::NoOp("Input".to_string()),
                ]),
            },

            FastTypedExpr::Id(_fet, identifier) => match self.scope_bindings.get(identifier) {
                None => panic!("Unbound variable identifier {:?}", identifier),
                Some(offset) => StepResult::Terminal(vec![
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *offset)),
                    Instr::NoOp("Id".to_string()),
                ]),
            },

            // unary ops
            FastTypedExpr::UnOp(t, op, exp) => {
                let constructor = match op {
                    Op1::Add1 | Op1::Sub1 | Op1::Not => UnOpConstructor::Other(*op),
                    Op1::Print => UnOpConstructor::Print {
                        flag: fast_type_to_flag(*t, &mut state.type_map),
                        rsp_offset: self.rsp_offset,
                    },
                };

                // compile exp
                StepResult::Nonterminal(NewStackStatePlus::new(
                    vec![self.split(exp)],
                    InstrsConstructor::UnOp(constructor),
                ))
            }

            FastTypedExpr::BinOp(_, op, b, a) => {
                let mut rsp_offset = self.rsp_offset;
                let mut to_compile: Vec<WrappedFTE> = Vec::with_capacity(2);

                let (label_true, label_finish) = match op {
                    Op2::Greater | Op2::GreaterEqual | Op2::Equal | Op2::LessEqual | Op2::Less => (
                        state.increment_counter(self.label_name),
                        state.increment_counter(self.label_name),
                    ),
                    _ => (-1, -1),
                };

                // compute the value of a into RAX
                to_compile.push(self.split_plus(a, rsp_offset));

                // store that value on the stack
                rsp_offset -= SIZEOF_I_64; // get 4 bytes of space on the stack

                // this computes b, and stores it in RAX. since we adjusted rsp_offset,
                // our stored value of a is still at a_rsp_offset
                to_compile.push(self.split_plus(b, rsp_offset));

                StepResult::Nonterminal(NewStackStatePlus::new(
                    to_compile,
                    InstrsConstructor::BinOp {
                        rsp_offset: self.rsp_offset,
                        op: *op,
                        label_name: self.label_name,
                        label_true,
                        label_finish,
                    },
                ))
            }

            FastTypedExpr::Let(_, bindings, final_expr) => {
                let num_bindings = bindings.len();
                let mut to_compile: Vec<_> = Vec::with_capacity(num_bindings + 1);
                let mut id_rsp_offsets: Vec<_> = Vec::with_capacity(num_bindings);
                let mut curr_rsp_offset = self.rsp_offset;
                let mut curr_let_binding = self.scope_bindings;
                let mut in_this_let: im::HashSet<&'a str> = im::HashSet::new();
                // evaluate in order using lexical scoping

                for (id, exp) in bindings {
                    // check for duplicates
                    match in_this_let.insert(id) {
                        None => (),
                        Some(_) => panic!("Duplicate binding"),
                    };

                    // compute the value of exp into RAX
                    to_compile.push(WrappedFTE {
                        fte: exp,
                        scope_bindings: curr_let_binding.clone(),
                        struct_layouts: self.struct_layouts,
                        rsp_offset: curr_rsp_offset,
                        label_counter: self.label_counter,
                        label_name: self.label_name,
                    });

                    // store that value on the stack
                    curr_rsp_offset -= SIZEOF_I_64; // get 4 bytes of space on the stack
                    let id_rsp_offset = curr_rsp_offset;
                    id_rsp_offsets.push(id_rsp_offset); // will need to add the instructions to compile this when constructing

                    // bind id to that location on the stack (in doing so, to that result)
                    curr_let_binding.insert(id, id_rsp_offset);
                }

                // evaluate the final expression after all the bindings into RAX
                to_compile.push(WrappedFTE {
                    fte: final_expr,
                    scope_bindings: curr_let_binding,
                    struct_layouts: self.struct_layouts,
                    rsp_offset: curr_rsp_offset,
                    label_counter: self.label_counter,
                    label_name: self.label_name,
                });

                StepResult::Nonterminal(NewStackStatePlus::new(
                    to_compile,
                    InstrsConstructor::Let { id_rsp_offsets },
                ))
            }

            FastTypedExpr::If(_, cond, if_true, if_false) => {
                let label_name: &'a str = self.label_name;

                StepResult::Nonterminal(NewStackStatePlus::new(
                    vec![self.split(cond), self.split(if_false), self.split(if_true)],
                    InstrsConstructor::If {
                        label_name,
                        left_label: state.increment_counter(label_name),
                        if_finish_label: state.increment_counter(label_name),
                    },
                ))
            }

            FastTypedExpr::Set(_, identifier, expr) => {
                // get the rsp offset where this variable is stored
                let id_rsp_offset = match self.scope_bindings.get(identifier) {
                    None => panic!("Unbound variable identifier {:?}", identifier),
                    Some(offset) => *offset,
                };

                StepResult::Nonterminal(NewStackStatePlus::new(
                    vec![self.split(expr)],
                    InstrsConstructor::Set(id_rsp_offset),
                ))
            }

            FastTypedExpr::Block(_, block) => {
                if block.len() == 0 {
                    panic!("Invalid: Empty Block");
                }
                StepResult::Nonterminal(NewStackStatePlus::new(
                    // compile each expression
                    block.into_iter().map(|fte| self.split(fte)).collect(),
                    InstrsConstructor::Block,
                ))
            }

            FastTypedExpr::RepeatUntil(_, body, stop_cond) => {
                let body_label = state.increment_counter(self.label_name);
                let id_rsp_offset = self.rsp_offset - SIZEOF_I_64;

                StepResult::Nonterminal(NewStackStatePlus::new(
                    vec![self.split(body), self.split_plus(stop_cond, id_rsp_offset)],
                    InstrsConstructor::RepeatUntil {
                        label_name: self.label_name,
                        body_label,
                        id_rsp_offset,
                    },
                ))
            }

            FastTypedExpr::Call(_ret_type, fun_name_or_ptr, args) => {
                /*
                foo(a, b) =>
                |    a        | <-- RSP + 16
                |    b        | <-- RSP + 8
                | return addr | <-- RSP
                */

                let (fun_label, is_in_rax) = match **fun_name_or_ptr {
                    // immediate names go as strings
                    FastTypedExpr::FunName(_, fun_name) => {
                        (FunctionLabel::Custom(fun_name.to_owned()), false) // TODO CLONE
                    }

                    // pointers should be compiled into rax
                    ref fexpr => {
                        if let ExprType::FunctionPointer(_, _) =
                            fexpr.extract_type().to_expr_type_ref(&state.type_map)
                        {
                            (FunctionLabel::Pointer(Reg::RAX), true)
                        } else {
                            unreachable!("already typechecks")
                        }
                    }
                };

                let mut to_compile: Vec<WrappedFTE> = Vec::new();
                let mut curr_rsp_offset = self.rsp_offset;
                // let mut id_rsp_offsets: Vec<i32> = Vec::new();

                let _ptr_rsp_offset = if is_in_rax {
                    // compile function pointer
                    to_compile.push(self.split_plus(fun_name_or_ptr, curr_rsp_offset));

                    // push pointer onto stack
                    curr_rsp_offset -= SIZEOF_I_64;
                    // id_rsp_offsets.push(curr_rsp_offset);
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
                    to_compile.push(self.split_plus(arg, curr_rsp_offset));

                    // store this argument on the stack
                    curr_rsp_offset -= SIZEOF_I_64;
                    // id_rsp_offsets.push(curr_rsp_offset);
                }

                if to_compile.is_empty() {
                    // this is actually terminal-- a function pointer which is a string, with no arguments
                    return StepResult::Terminal({
                        let mut inst_to_call = Vec::new();
                        let mut curr_rsp_offset = self.rsp_offset;

                        // what rsp would be after pushing args
                        let unadjusted_call_rsp =
                            curr_rsp_offset - (SIZEOF_I_64 * args.len() as i32);

                        // preemptively adjust rsp so that it will 8 byte aligned, mod 16, after pushing args
                        curr_rsp_offset +=
                            compute_aligned_rsp_offset(unadjusted_call_rsp) - unadjusted_call_rsp;

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

                        inst_to_call.push(Instr::NoOp("Call".to_string()));

                        inst_to_call
                    });
                }

                let constructor = InstrsConstructor::Call {
                    fun_label,
                    is_in_rax,
                    rsp_offset: self.rsp_offset,
                };

                StepResult::Nonterminal(NewStackStatePlus::new(to_compile, constructor))
            }

            FastTypedExpr::Null(_) => StepResult::Terminal(vec![
                Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)),
                Instr::NoOp("Null".to_string()),
            ]),

            FastTypedExpr::Alloc(fast_expr_type) => {
                let instructions = {
                    let mut instructions = Vec::new();
                    let type_string = match fast_expr_type.to_expr_type_ref(&state.type_map) {
                        ExprType::StructPointer(n) => {
                            let num_to_name_map = STRUCT_NUM_TO_NAME.lock().unwrap();
                            let res = num_to_name_map.get(&n);
                            match res {
                                Some(s) => s.to_string(),
                                None => unreachable!("Broken structure enumeration."),
                            }
                        }

                        other_type => {
                            unreachable!("Attempted to create a {:?} pointer.", other_type)
                        }
                    };
                    let size = match self.struct_layouts.get(type_string.as_str()) {
                        Some(FastStructLayout::Layout(layout_dict)) => layout_dict.len() as i32,
                        None => unreachable!("Broken structure dictionary."),
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

                    instructions.push(Instr::NoOp("Alloc".to_string()));
                    instructions
                };

                StepResult::Terminal(instructions)
            }

            FastTypedExpr::Update(_, ptr, field_name, new_val) => {
                let offset = get_offset(
                    ptr.extract_type().to_expr_type_ref(&state.type_map),
                    field_name,
                    self.struct_layouts,
                );

                let curr_rsp_offset = self.rsp_offset - SIZEOF_I_64;
                StepResult::Nonterminal(NewStackStatePlus::new(
                    vec![self.split(new_val), self.split_plus(ptr, curr_rsp_offset)],
                    InstrsConstructor::Update {
                        offset,
                        curr_rsp_offset,
                    },
                ))
            }

            FastTypedExpr::Lookup(_, typed_expr, field_name) => {
                let ptr_type = typed_expr.extract_type().to_expr_type_ref(&state.type_map);
                let offset = get_offset(ptr_type, field_name, self.struct_layouts);

                StepResult::Nonterminal(NewStackStatePlus::new(
                    vec![self.split(typed_expr)],
                    InstrsConstructor::Lookup { offset },
                ))
            }

            FastTypedExpr::Unit => StepResult::Terminal(vec![Instr::NoOp("Unit".to_string())]),

            FastTypedExpr::FunName(_expr_type, name) => StepResult::Terminal(vec![
                Instr::Lea(Val::Reg(Reg::RAX), Val::Global((*name).to_owned())),
                Instr::NoOp("FunName".to_string()),
            ]),

            FastTypedExpr::ArrayAlloc(_elem_t, len) => {
                StepResult::Nonterminal(NewStackStatePlus::new(
                    vec![self.split(len)],
                    InstrsConstructor::ArrayAlloc {
                        rsp_offset: self.rsp_offset,
                    },
                ))
            }

            FastTypedExpr::ArrayLen(_elem_t, arr_addr) => StepResult::Nonterminal(
                NewStackStatePlus::new(vec![self.split(arr_addr)], InstrsConstructor::ArrayLen),
            ),

            FastTypedExpr::ArrayLookup(_elem_t, arr, ind_expr) => {
                let mut curr_rsp_offset = self.rsp_offset;
                let mut to_compile = vec![self.split_plus(arr, curr_rsp_offset)];

                // put arr on top of stack
                curr_rsp_offset -= SIZEOF_I_64; // get 8 bytes of space on the stack
                to_compile.push(self.split_plus(ind_expr, curr_rsp_offset));

                StepResult::Nonterminal(NewStackStatePlus::new(
                    to_compile,
                    InstrsConstructor::ArrayLookup {
                        rsp_offset: self.rsp_offset,
                    },
                ))
            }

            FastTypedExpr::ArrayUpdate(_elem_t, arr, ind_expr, new_val) => {
                let mut curr_rsp_offset = self.rsp_offset;
                let mut to_compile: Vec<WrappedFTE> = Vec::with_capacity(3);

                to_compile.push(self.split_plus(arr, curr_rsp_offset));

                // If not null, put arr on top of stack
                curr_rsp_offset -= SIZEOF_I_64; // get 8 bytes of space on the stack

                to_compile.push(self.split_plus(ind_expr, curr_rsp_offset));
                // index now in rax

                to_compile.push(self.split_plus(new_val, curr_rsp_offset));

                StepResult::Nonterminal(NewStackStatePlus::new(
                    to_compile,
                    InstrsConstructor::ArrayUpdate {
                        rsp_offset: self.rsp_offset,
                    },
                ))
            }
        }
    }
}

impl<'a, 'b> WrappedFTE<'a, 'b> {
    /// Split a child using a given FastTypedExpr, cloning the scope_bindings, label_counter, label_name
    fn split(&self, fte: &'b FastTypedExpr<'a>) -> WrappedFTE<'a, 'b> {
        WrappedFTE {
            fte,
            scope_bindings: self.scope_bindings.clone(),
            struct_layouts: self.struct_layouts,
            rsp_offset: self.rsp_offset,
            label_counter: self.label_counter,
            label_name: self.label_name,
        }
    }

    fn split_plus(&self, fte: &'b FastTypedExpr<'a>, rsp_offset: i32) -> WrappedFTE<'a, 'b> {
        WrappedFTE {
            fte,
            scope_bindings: self.scope_bindings.clone(),
            struct_layouts: self.struct_layouts,
            rsp_offset,
            label_counter: self.label_counter,
            label_name: self.label_name,
        }
    }
}

fn get_offset<'a, 'b>(
    ptr_type: &ExprType,
    field_name: &str,
    struct_layouts: &'b im::HashMap<&'a str, FastStructLayout<'a>>,
) -> i32 {
    let struct_enum = {
        match ptr_type {
            ExprType::StructPointer(struct_enum) => struct_enum,
            other => unreachable!("Disallowed access a {:?} pointer", other),
        }
    };
    match STRUCT_NUM_TO_NAME.lock().unwrap().get(struct_enum) {
        None => unreachable!("Broken structure enumeration."),

        // struct name exists; find the field offset
        Some(struct_name) => match struct_layouts.get(struct_name.as_str()) {
            None => unreachable!("Broken structure dictionary."),
            Some(FastStructLayout::Layout(layout_dict)) => match layout_dict.get(field_name) {
                None => unreachable!("Missing field."),
                Some(offset) => *offset,
            },
        },
    }
}

fn compile_fn<'a, 'b>(
    f: &'b FastTypedFunction<'a>,
    struct_layouts: &'b im::HashMap<&'a str, FastStructLayout<'a>>,
    compile_state: &mut GlobalCompileState<'a, 'b>,
) -> Vec<Instr> {
    let FastTypedFunction::Fun(fun_name, FastFunSignature::Sig(_ret_type, args), typed_body) = f;

    // add the label for our code
    let mut instrs = vec![
        Instr::Align(8), // to use as function pointers
        Instr::AddLabel(fun_name.to_string()),
    ];

    if *fun_name == ENTRYPOINT_LABEL {
        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0)))
    }

    let init_rsp_offset = 0; // the body of a function expects RSP has been adjusted

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
        scope_bindings.insert(*arg_name, arg_offset_above_rsp);
    }

    let wrapped_body: WrappedFTE = WrappedFTE {
        fte: typed_body,
        scope_bindings,
        struct_layouts,
        rsp_offset: init_rsp_offset,
        label_counter: (),
        label_name: fun_name,
    };

    let mut stack: crate::alt_stack::IterativeStack<LocalCompileState, GlobalCompileState> =
        crate::alt_stack::IterativeStack::new(compile_state);

    // RSP should be the same after running these instr as it was before
    instrs.extend(stack.iterate(wrapped_body));

    // return execution
    instrs.push(Instr::Ret);

    instrs
}

pub fn compile_prog<'a>(tp: &mut FastTypedProg<'a>) -> Vec<Instr> {
    let FastTypedProg::Program(
        body_type,
        type_set,
        _struct_sigs,
        struct_layouts,
        typed_funs,
        typed_e,
    ) = tp;

    let prog_ret_type = body_type.to_expr_type(&type_set);

    let mut global_compile_state: GlobalCompileState = GlobalCompileState {
        type_map: type_set,
        label_map: std::collections::HashMap::new(),
    };

    let mut all_instrs = Vec::new();

    for typed_fun in typed_funs {
        all_instrs.extend(compile_fn(
            typed_fun,
            struct_layouts,
            &mut global_compile_state,
        ));
    }

    let took_typed_e = std::mem::replace(typed_e, FastTypedExpr::__INTERNAL);

    // compile program body, with the label __main
    let main = FastTypedFunction::Fun(
        MAIN_LABEL,
        FastFunSignature::Sig(prog_ret_type.clone(), vec![(ExprType::Int, "input")]), // input: int -> body_type
        took_typed_e,
    );
    all_instrs.extend(compile_fn(
        &main,
        &struct_layouts,
        &mut global_compile_state,
    ));

    // compile the entrypoint, which loads RDI (input) onto the stack, then calls __main
    let entrypoint = FastTypedFunction::Fun(
        // (print (main input : int ) : body_type ) : body_type
        ENTRYPOINT_LABEL,
        FastFunSignature::Sig(prog_ret_type, Vec::new()), // unit -> body_type
        FastTypedExpr::UnOp(
            *body_type,
            Op1::Print,
            Box::new(FastTypedExpr::Call(
                *body_type,
                Box::new(FastTypedExpr::FunName(*body_type, MAIN_LABEL)),
                vec![FastTypedExpr::RDInput],
            )),
        ),
    );
    all_instrs.extend(compile_fn(
        &entrypoint,
        &struct_layouts,
        &mut global_compile_state,
    ));

    // a runtime error causes us to jump here before reaching the SnekPrint call
    // since this function doesn't typecheck in the current type system, write it in ASM
    all_instrs.push(Instr::AddLabel(ERROR_LABEL.to_string()));
    all_instrs.push(Instr::Call(FunctionLabel::SnekError));
    all_instrs.push(Instr::Ret);

    let FastTypedFunction::Fun(
        _,
        FastFunSignature::Sig(_prog_ret_type, _), // input: int -> body_type
        took_typed_e,
    ) = main;

    *typed_e = took_typed_e;

    all_instrs
}

trait ConstructorPlus<To> {
    type State<'b>;

    fn construct<'b>(&mut self, parsed: &mut Vec<To>, stack: &mut Self::State<'b>) -> To;
}

impl<'a, 'b, From, To, C> StackStateAble<GlobalCompileState<'a, 'b>>
    for NewStackStatePlus<From, To, C>
where
    C: ConstructorPlus<To, State<'b> = GlobalCompileState<'a, 'b>>,
{
    type From = From;

    type To = To;

    fn consume_plus(
        &mut self,
        expr: Self::To,
        state: &mut GlobalCompileState<'a, 'b>,
    ) -> StackRetval<Self::From, Self::To> {
        self.parsed.push(expr);
        match self.unparsed.pop() {
            None => {
                assert!(self.len == self.parsed.len()); // as SimpleStackState provides no ability to produce unparsed
                StackRetval::Done(self.constructor.construct(&mut self.parsed, state))
            }
            Some(parse_next) => StackRetval::KeepGoing(parse_next),
        }
    }

    fn get_next(&mut self) -> StackRetval<Self::From, Self::To> {
        StackRetval::KeepGoing(self.unparsed.pop().unwrap())
    }
}
