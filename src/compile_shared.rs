use std::fmt::{write, Display};

use crate::{consts::*, structs::ExprType, structs::*};

/**
 * Helper Function
 */

pub fn increment_counter(counter: &mut i64) -> i64 {
    *counter += 1;
    *counter
}

pub fn generate_label<T: Display>(label_name: T, label_counter: i64) -> String {
    return format!("{}_label_{}", label_name, label_counter);
}

pub fn compute_aligned_rsp_offset(rsp_offset: i32) -> i32 {
    return (rsp_offset / 16) * 16 - 8;
}

pub fn type_to_flag(t: &ExprType) -> i32 {
    match t {
        ExprType::Int => INT_TYPE_FLAG,
        ExprType::Bool => BOOL_TYPE_FLAG,
        ExprType::Unit => UNIT_TYPE_FLAG,
        ExprType::StructPointer(i) => *i,
        ExprType::FunctionPointer(_arg_types, _ret_type) => todo!(),
        ExprType::Array(expr_type) => match **expr_type {
            ExprType::Int => 10_000_000,
            _ => todo!(),
        },
    }
}

/**
 * To String Functions
 */

pub fn instr_to_string(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("\tmov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("\tadd {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("\tsub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("\timul {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::LXOR(dst, src) => format!("\txor {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::LOR(dst, src) => format!("\tor {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::LAND(dst, src) => format!("\tand {}, {}", val_to_str(dst), val_to_str(src)),
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
        Instr::Align(alignment) => format!("align {}", alignment),
        Instr::NoOp(_) => "".to_string(),
    }
}

pub fn fn_to_str(f: &FunctionLabel) -> String {
    match f {
        FunctionLabel::SnekPrint => "snek_print".to_string(),
        FunctionLabel::SnekError => "snek_error".to_string(),
        FunctionLabel::Custom(name) => name.to_string(),
        FunctionLabel::Pointer(reg) => reg_to_str(reg),
    }
}

pub fn reg_to_str(r: &Reg) -> String {
    match r {
        Reg::RAX => "rax".to_string(),
        Reg::RBX => "rbx".to_string(),
        Reg::RDX => "rdx".to_string(),
        Reg::RSP => "rsp".to_string(),
        Reg::RSI => "rsi".to_string(),
        Reg::RDI => "rdi".to_string(),
    }
}

pub fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(r) => reg_to_str(r),
        Val::RegOffset(r, off) => format!("[{} + {}]", reg_to_str(r), off),
        Val::Imm(x) => format!("{}", x),
        Val::Global(global_name) => format!("{}", global_name),
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::RAX => write!(f, "rax"),
            Reg::RBX => write!(f, "rbx"),
            Reg::RDX => write!(f, "rdx"),
            Reg::RSP => write!(f, "rsp"),
            Reg::RSI => write!(f, "rsi"),
            Reg::RDI => write!(f, "rdi"),
        }
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Reg(reg) => reg.fmt(f),
            Val::RegOffset(r, off) => write!(f, "[{} + {}]", r, off),
            Val::Imm(x) => write!(f, "{}", x),
            Val::Global(global_name) => write!(f, "{}", global_name),
        }
    }
}

impl Display for FunctionLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionLabel::SnekPrint => write!(f, "snek_print"),
            FunctionLabel::SnekError => write!(f, "snek_error"),
            FunctionLabel::Custom(name) => write!(f, "{}", name),
            FunctionLabel::Pointer(reg) => write!(f, "{}", reg),
        }
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::IMov(dst, src) => write!(f, "\tmov {}, {}", dst, src),
            Instr::IAdd(dst, src) => write!(f, "\tadd {}, {}", dst, src),
            Instr::ISub(dst, src) => write!(f, "\tsub {}, {}", dst, src),
            Instr::IMul(dst, src) => write!(f, "\timul {}, {}", dst, src),
            Instr::LXOR(dst, src) => write!(f, "\txor {}, {}", dst, src),
            Instr::LOR(dst, src) => write!(f, "\tor {}, {}", dst, src),
            Instr::LAND(dst, src) => write!(f, "\tand {}, {}", dst, src),
            Instr::Compare(dst, src) => write!(f, "\tcmp {}, {}", dst, src),
            Instr::Call(function) => write!(f, "\tcall {}", function),
            Instr::AddLabel(label) => write!(f, "{}:", label),
            Instr::Jump(label) => write!(f, "\tjmp {}", label),
            Instr::JumpGreater(label) => write!(f, "\tjg {}", label),
            Instr::JumpGreaterEqual(label) => write!(f, "\tjge {}", label),
            Instr::JumpEqual(label) => write!(f, "\tje {}", label),
            Instr::JumpLessEqual(label) => write!(f, "\tjle {}", label),
            Instr::JumpLess(label) => write!(f, "\tjl {}", label),
            Instr::JumpOverflow(label) => write!(f, "\tjo {}", label),
            Instr::Ret => write!(f, "\tret"),
            Instr::Lea(dst, src) => write!(f, "\tlea {}, [rel {}]", dst, src),
            Instr::Align(alignment) => write!(f, "align {}", alignment),
            Instr::NoOp(_) => write!(f, ""),
        }
    }
}

pub struct ProgInstr<T: Display> {
    pub instrs: Vec<Instr>,
    pub struct_serializer: T,
}

impl<T: Display> Display for ProgInstr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ProgInstr {
            instrs,
            struct_serializer,
        } = self;

        write!(
            f,
            "
section .data
global structHashmap
structHashmap db \"{struct_serializer}\", 0

section .bss
global {BUFFER_NAME}
{BUFFER_NAME}: resb {BUFFER_SIZE} ; reserve {BUFFER_SIZE} bytes for array

section .text

extern snek_print
extern snek_error

global our_code_starts_here
    "
        )?;

        for instr in instrs {
            if let Instr::NoOp(_) = instr {
                continue;
            }
            write!(f, "{}\n", instr)?;
        }

        Ok(())
    }
}

pub fn emit_program(all_instrs: &Vec<Instr>, struct_serializer: impl ToString) -> String {
    let instrs_string = all_instrs
        .iter()
        .map(|instr| format!("  {}", instr_to_string(&instr)))
        .collect::<Vec<String>>()
        .join("\n");

    let serialized = struct_serializer.to_string();

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
