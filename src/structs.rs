use im::HashMap;
use std::fmt;

use crate::semantics::struct_type_enum_to_name;

/* --- Types --- */

#[derive(Clone, Copy, PartialEq)]
pub enum ExprType {
    Int,
    Bool,
    // new to egg-eater
    StructPointer(i32), // pointer to a struct, whose name is contained in STRUCT_NUM_TO_NAME[i32]
}

impl fmt::Debug for ExprType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprType::Int => write!(f, "Int"),
            ExprType::Bool => write!(f, "Bool"),
            ExprType::StructPointer(struct_type_enum) => {
                match struct_type_enum_to_name(*struct_type_enum) {
                    Some(struct_name) => write!(f, "StructPointer({})", struct_name),
                    None => write!(f, "Unrecognized StructPointer({})", struct_type_enum),
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum FunSignature {
    // to insert into function signature hashmap, for type_check_expr
    Sig(ExprType, Vec<(ExprType, String)>),
}

#[derive(Clone, Debug)]
pub enum StructSignature {
    Sig(Vec<(ExprType, String)>),
}

pub fn extract_type(t: &TypedExpr) -> ExprType {
    match t {
        TypedExpr::Number(_) => ExprType::Int,
        TypedExpr::RDInput => ExprType::Int,
        TypedExpr::Boolean(_) => ExprType::Bool,
        TypedExpr::Input => ExprType::Int,
        TypedExpr::Id(expr_type, _) => *expr_type,
        TypedExpr::Let(expr_type, _, _) => *expr_type,
        TypedExpr::UnOp(expr_type, _, _) => *expr_type,
        TypedExpr::BinOp(expr_type, _, _, _) => *expr_type,
        TypedExpr::If(expr_type, _, _, _) => *expr_type,
        TypedExpr::RepeatUntil(expr_type, _, _) => *expr_type,
        TypedExpr::Set(expr_type, _, _) => *expr_type,
        TypedExpr::Block(expr_type, _) => *expr_type,
        TypedExpr::Call(expr_type, _, _) => *expr_type,
        TypedExpr::Null(expr_type) => *expr_type,
        TypedExpr::Alloc(expr_type) => *expr_type,
        TypedExpr::Update(expr_type, _, _, _) => *expr_type,
        TypedExpr::Lookup(expr_type, _, _) => *expr_type,
    }
}

/* --- Semantics --- */

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Op1 {
    Add1,
    Sub1,
    Not,
    Print,
}

impl fmt::Display for Op1 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            Op1::Add1 => "add1",
            Op1::Sub1 => "sub1",
            Op1::Print => "print",
            Op1::Not => "not",
        };
        write!(f, "{}", output)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl fmt::Display for Op2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            Op2::Plus => "+",
            Op2::Minus => "-",
            Op2::Times => "*",
            Op2::Equal => "==",
            Op2::Greater => ">",
            Op2::GreaterEqual => ">=",
            Op2::Less => "<",
            Op2::LessEqual => "<=",
        };
        write!(f, "{}", output)
    }
}

/* --- Parsed, not Type-checked --- */

#[derive(Debug)]
pub enum Expr {
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
    Call(String, Vec<Expr>),
    /* --- new to egg-eater --- */
    Null(String), // null pointer to a struct type specified by name
    Alloc(String),
    Update(Box<Expr>, String, Box<Expr>),
    Lookup(Box<Expr>, String),
}

#[derive(Debug)]
pub enum UserFunction {
    UserFun(String, FunSignature, Expr),
}

#[derive(Debug)]
pub enum UserStruct {
    /* UserStruct(type_name, <(t1, field1name, ...)>) */
    UserStruct(String, StructSignature),
}

#[derive(Debug)]
pub enum Prog {
    Program(Vec<UserStruct>, Vec<UserFunction>, Expr),
}

/* --- Type Checked --- */

#[derive(Clone, PartialEq)]
pub enum TypedExpr {
    Number(i32),
    Boolean(bool),
    Id(ExprType, String),
    Let(ExprType, Vec<(String, TypedExpr)>, Box<TypedExpr>),
    UnOp(ExprType, Op1, Box<TypedExpr>),
    BinOp(ExprType, Op2, Box<TypedExpr>, Box<TypedExpr>),
    If(ExprType, Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>),
    RepeatUntil(ExprType, Box<TypedExpr>, Box<TypedExpr>),
    Set(ExprType, String, Box<TypedExpr>),
    Block(ExprType, Vec<TypedExpr>),
    Call(ExprType, String, Vec<TypedExpr>), // ExprType is return type of call
    Input,
    RDInput,
    /* --- New to egg-eater --- */
    Null(ExprType),
    Alloc(ExprType),
    Update(ExprType, Box<TypedExpr>, String, Box<TypedExpr>),
    Lookup(ExprType, Box<TypedExpr>, String),
}

pub enum TypedFunction {
    // for use in a program
    Fun(String, FunSignature, TypedExpr),
}

#[derive(Clone, Debug)]
pub enum StructLayout {
    Layout(HashMap<String, i32>),
}

pub enum TypedProg {
    // everything which will be compiled
    // structs aren't compiled-- they're reduced to sizes (alloc) and offsets (lookup, update)
    Program(
        ExprType,
        HashMap<String, StructSignature>,
        HashMap<String, StructLayout>,
        Vec<TypedFunction>,
        TypedExpr,
    ),
}

/* --- For Compiling --- */

#[derive(Debug)]
pub enum FunctionLabel {
    Custom(String),
    SnekPrint,
    SnekError,
}

#[derive(Debug)]
pub enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
    Global(String), // the name of a global, register containing offset
}

#[derive(Debug)]
pub enum Reg {
    RAX,
    RBX,
    RDX,
    RSP,
    RSI,
    RDI,
}

#[derive(Debug)]
pub enum Instr {
    IMov(Val, Val), // mov dest, source
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    LXOR(Val, Val), // Negates the value at val
    Compare(Val, Val),
    Call(FunctionLabel),
    AddLabel(String),
    Jump(String),
    JumpGreater(String),
    JumpGreaterEqual(String),
    JumpEqual(String),
    JumpLessEqual(String),
    JumpLess(String),
    JumpOverflow(String),
    Ret,
    Lea(Val, Val), // loads the efective address of the second val into the first
}
