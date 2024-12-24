use im::HashMap;
use std::fmt;

use crate::{semantics::struct_type_enum_to_name, typecheck_fast::TypeCheckState};

/* --- Types --- */

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum ExprType {
    Int,
    Bool,
    // new to egg-eater
    StructPointer(i32), // pointer to a struct, whose name is contained in STRUCT_NUM_TO_NAME[i32]
    // new to bet
    Unit,
    FunctionPointer(Vec<ExprType>, Box<ExprType>), // arg types, then ret type
    Array(Box<ExprType>),
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
            ExprType::Unit => write!(f, "Unit"),
            ExprType::FunctionPointer(arg_types, ret_type) => {
                write!(f, "FunctionPointer({:?} -> {:?})", arg_types, ret_type)
            }
            ExprType::Array(expr_type) => write!(f, "Array of {:?}", expr_type),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunSignature {
    // to insert into function signature hashmap, for type_check_expr
    Sig(ExprType, Vec<(ExprType, String)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructSignature {
    Sig(Vec<(ExprType, String)>),
}

// pub fn fast_extract_type(t: &FastTypedExpr) -> ExprType {
//     match t {
//         FastTypedExpr::Number(_) => ExprType::Int,
//         FastTypedExpr::RDInput => ExprType::Int,
//         FastTypedExpr::Boolean(_) => ExprType::Bool,
//         FastTypedExpr::Input => ExprType::Int,
//         FastTypedExpr::Id(expr_type, _) => expr_type.clone(),
//         FastTypedExpr::Let(expr_type, _, _) => expr_type.clone(),
//         FastTypedExpr::UnOp(expr_type, _, _) => expr_type.clone(),
//         FastTypedExpr::BinOp(expr_type, _, _, _) => expr_type.clone(),
//         FastTypedExpr::If(expr_type, _, _, _) => expr_type.clone(),
//         FastTypedExpr::RepeatUntil(expr_type, _, _) => expr_type.clone(),
//         FastTypedExpr::Set(expr_type, _, _) => expr_type.clone(),
//         FastTypedExpr::Block(expr_type, _) => expr_type.clone(),
//         FastTypedExpr::Call(expr_type, _, _) => expr_type.clone(),
//         FastTypedExpr::Null(expr_type) => expr_type.clone(),
//         FastTypedExpr::Alloc(expr_type) => expr_type.clone(),
//         FastTypedExpr::Update(expr_type, _, _, _) => expr_type.clone(),
//         FastTypedExpr::Lookup(expr_type, _, _) => expr_type.clone(),
//         // new to bet
//         FastTypedExpr::Unit => ExprType::Unit,
//         FastTypedExpr::FunName(expr_type, _) => expr_type.clone(),
//         FastTypedExpr::ArrayAlloc(expr_type, _) => expr_type.clone(),
//         FastTypedExpr::ArrayLookup(expr_type, _, _) => expr_type.clone(),
//         FastTypedExpr::ArrayUpdate(expr_type, _, _, _) => expr_type.clone(),
//         FastTypedExpr::ArrayLen(expr_type, _) => expr_type.clone(),
//     }
// }

pub fn extract_type(t: &TypedExpr) -> ExprType {
    // TODO CLONE
    match t {
        TypedExpr::Number(_) => ExprType::Int,
        TypedExpr::RDInput => ExprType::Int,
        TypedExpr::Boolean(_) => ExprType::Bool,
        TypedExpr::Input => ExprType::Int,
        TypedExpr::Id(expr_type, _) => expr_type.clone(),
        TypedExpr::Let(expr_type, _, _) => expr_type.clone(),
        TypedExpr::UnOp(expr_type, _, _) => expr_type.clone(),
        TypedExpr::BinOp(expr_type, _, _, _) => expr_type.clone(),
        TypedExpr::If(expr_type, _, _, _) => expr_type.clone(),
        TypedExpr::RepeatUntil(expr_type, _, _) => expr_type.clone(),
        TypedExpr::Set(expr_type, _, _) => expr_type.clone(),
        TypedExpr::Block(expr_type, _) => expr_type.clone(),
        TypedExpr::Call(expr_type, _, _) => expr_type.clone(),
        TypedExpr::Null(expr_type) => expr_type.clone(),
        TypedExpr::Alloc(expr_type) => expr_type.clone(),
        TypedExpr::Update(expr_type, _, _, _) => expr_type.clone(),
        TypedExpr::Lookup(expr_type, _, _) => expr_type.clone(),
        // new to bet
        TypedExpr::Unit => ExprType::Unit,
        TypedExpr::FunName(expr_type, _) => expr_type.clone(),
        TypedExpr::ArrayAlloc(expr_type, _) => ExprType::Array(Box::new(expr_type.clone())),
        TypedExpr::ArrayLookup(expr_type, _, _) => expr_type.clone(),
        TypedExpr::ArrayUpdate(expr_type, _, _, _) => expr_type.clone(),
        TypedExpr::ArrayLen(expr_type, _) => expr_type.clone(),
    }
}

impl<'a> FastTypedExpr<'a> {
    pub fn extract_type(&self) -> FastExprType {
        match self {
            // base
            FastTypedExpr::Input | FastTypedExpr::Number(_) | FastTypedExpr::RDInput => {
                FastExprType::Int
            }
            FastTypedExpr::Boolean(_) => FastExprType::Bool,
            FastTypedExpr::Unit => FastExprType::Unit,

            // recursive
            FastTypedExpr::Id(expr_type, _) => expr_type.clone(),
            FastTypedExpr::Let(expr_type, _, _) => expr_type.clone(),
            FastTypedExpr::UnOp(expr_type, _, _) => expr_type.clone(),
            FastTypedExpr::BinOp(expr_type, _, _, _) => expr_type.clone(),
            FastTypedExpr::If(expr_type, _, _, _) => expr_type.clone(),
            FastTypedExpr::RepeatUntil(expr_type, _, _) => expr_type.clone(),
            FastTypedExpr::Set(expr_type, _, _) => expr_type.clone(),
            FastTypedExpr::Block(expr_type, _) => expr_type.clone(),
            FastTypedExpr::Call(expr_type, _, _) => expr_type.clone(),
            FastTypedExpr::Null(expr_type) => expr_type.clone(),
            FastTypedExpr::Alloc(expr_type) => expr_type.clone(),
            FastTypedExpr::Update(expr_type, _, _, _) => expr_type.clone(),
            FastTypedExpr::Lookup(expr_type, _, _) => expr_type.clone(),
            // new to bet
            FastTypedExpr::FunName(expr_type, _) => expr_type.clone(),
            FastTypedExpr::ArrayLookup(expr_type, _, _) => expr_type.clone(),
            FastTypedExpr::ArrayUpdate(expr_type, _, _, _) => expr_type.clone(),
            FastTypedExpr::ArrayLen(expr_type, _) => expr_type.clone(),
            FastTypedExpr::ArrayAlloc(expr_type, _) => expr_type.clone(),
        }
    }
}

impl ExprType {
    pub fn is_base(&self) -> Option<FastExprType> {
        match self {
            ExprType::Int => {
                Some(FastExprType::Int)
            }
            ExprType::Bool => Some(FastExprType::Bool),
            ExprType::Unit => Some(FastExprType::Unit),
            _ => None,
        }
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
    Or,
    And,
}

impl fmt::Display for Op2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            Op2::Plus => "+",
            Op2::Minus => "-",
            Op2::Times => "*",
            Op2::Or => "||",
            Op2::And => "&&",
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

#[derive(Debug, PartialEq)]
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
    FunName(String),
    // TODO: Support function pointers
    Call(Box<Expr>, Vec<Expr>),
    /* --- new to egg-eater --- */
    Null(String), // null pointer to a struct type specified by name
    Alloc(String),
    Update(Box<Expr>, String, Box<Expr>),
    Lookup(Box<Expr>, String),
    Unit,                                         // TODO: do we want this to be an expression?
    ArrayLookup(Box<Expr>, Box<Expr>),            // arr[ind] => ArrayLookup(arr, ind)
    ArrayUpdate(Box<Expr>, Box<Expr>, Box<Expr>), // arr[ind] := new_val => ArrayUpdate(arr, ind, new_val)
    ArrayAlloc(ExprType, Box<Expr>), // new_arr(<type>, size) => ArrayAlloc(<type>, size)
    ArrayLen(Box<Expr>),             // arr_len(arr)
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

#[derive(Clone, PartialEq, Debug)]
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
    // Function Pointers
    FunName(ExprType, String), // (fun_type, fun_name) // fun_type is ret type & expr types
    Call(ExprType, Box<TypedExpr>, Vec<TypedExpr>), // (return_type, function_name_or_pointer, arguments)

    Input,
    RDInput,
    /* --- New to egg-eater --- */
    Null(ExprType),
    Alloc(ExprType),
    Update(ExprType, Box<TypedExpr>, String, Box<TypedExpr>),
    Lookup(ExprType, Box<TypedExpr>, String),
    /* --- New to bet --- */
    Unit,
    ArrayAlloc(ExprType, Box<TypedExpr>), // array pointer
    ArrayLookup(ExprType, Box<TypedExpr>, Box<TypedExpr>), // ArrayLookup(arr::array(t), ind::int)::t

    /// ArrayUpdate(arr::array(t), ind::int, elem::t)::t
    ArrayUpdate(ExprType, Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>),
    ArrayLen(ExprType, Box<TypedExpr>), // ArrayLen(int, Box<array>)
}

#[derive(Debug, PartialEq)]
pub enum TypedFunction {
    // for use in a program
    Fun(String, FunSignature, TypedExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructLayout {
    Layout(HashMap<String, i32>),
}

#[derive(Debug, PartialEq)]
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
    Pointer(Reg),
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
    Align(i32),
    IMov(Val, Val), // mov dest, source
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    LXOR(Val, Val),
    LOR(Val, Val),
    LAND(Val, Val),
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

/* More memory efficient Expressions */

#[derive(Debug, Clone)]
pub enum FastFunSignature<'a> {
    // to insert into function signature hashmap, for type_check_expr
    Sig(ExprType, Vec<(ExprType, &'a str)>),
}

#[derive(Clone, Debug)]
pub enum FastStructSignature<'a> {
    Sig(Vec<(ExprType, &'a str)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FastExpr<'a> {
    Number(i32),
    Boolean(bool),
    Id(&'a str),
    Let(Vec<(&'a str, FastExpr<'a>)>, Box<FastExpr<'a>>),
    UnOp(Op1, Box<FastExpr<'a>>),
    BinOp(Op2, Box<FastExpr<'a>>, Box<FastExpr<'a>>),
    If(Box<FastExpr<'a>>, Box<FastExpr<'a>>, Box<FastExpr<'a>>),
    RepeatUntil(Box<FastExpr<'a>>, Box<FastExpr<'a>>),
    Set(&'a str, Box<FastExpr<'a>>),
    Block(Vec<FastExpr<'a>>),
    Input,
    FunName(&'a str),
    // TODO: Support function pointers
    Call(Box<FastExpr<'a>>, Vec<FastExpr<'a>>),
    /* --- new to egg-eater --- */
    Null(&'a str), // null pointer to a struct type specified by name
    Alloc(&'a str),
    Update(Box<FastExpr<'a>>, &'a str, Box<FastExpr<'a>>),
    Lookup(Box<FastExpr<'a>>, &'a str),
    Unit, // TODO: do we want this to be an expression?
    ArrayLookup(Box<FastExpr<'a>>, Box<FastExpr<'a>>), // arr[ind] => ArrayLookup(arr, ind)
    ArrayUpdate(Box<FastExpr<'a>>, Box<FastExpr<'a>>, Box<FastExpr<'a>>), // arr[ind] := new_val => ArrayUpdate(arr, ind, new_val)
    ArrayAlloc(ExprType, Box<FastExpr<'a>>), // new_arr(<type>, size) => ArrayAlloc(<type>, size)
    ArrayLen(Box<FastExpr<'a>>),             // arr_len(arr)
    __INTERNAL,
}

#[derive(Debug, Clone)]
pub enum FastUserFunction<'a> {
    UserFun(&'a str, FastFunSignature<'a>, FastExpr<'a>),
}

#[derive(Debug, Clone)]
pub enum FastUserStruct<'a> {
    /* UserStruct(type_name, <(t1, field1name, ...)>) */
    UserStruct(&'a str, FastStructSignature<'a>),
}

#[derive(Debug, Clone)]
pub enum FastProg<'a> {
    Program(
        Vec<FastUserStruct<'a>>,
        Vec<FastUserFunction<'a>>,
        FastExpr<'a>,
    ),
}

/** --- `&str` Based Type Checking --- */

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum FastExprType {
    Int,
    Unit,
    Bool,
    Other(usize),
}

#[derive(Clone, PartialEq, Debug)]
pub enum FastTypedExpr<'a> {
    Number(i32),
    Boolean(bool),
    Id(FastExprType, &'a str),
    Let(
        FastExprType,
        Vec<(&'a str, FastTypedExpr<'a>)>,
        Box<FastTypedExpr<'a>>,
    ),
    UnOp(FastExprType, Op1, Box<FastTypedExpr<'a>>),
    BinOp(
        FastExprType,
        Op2,
        Box<FastTypedExpr<'a>>,
        Box<FastTypedExpr<'a>>,
    ),
    If(
        FastExprType,
        Box<FastTypedExpr<'a>>,
        Box<FastTypedExpr<'a>>,
        Box<FastTypedExpr<'a>>,
    ),
    RepeatUntil(FastExprType, Box<FastTypedExpr<'a>>, Box<FastTypedExpr<'a>>),
    Set(FastExprType, &'a str, Box<FastTypedExpr<'a>>),
    Block(FastExprType, Vec<FastTypedExpr<'a>>),
    // Function Pointers
    FunName(FastExprType, &'a str), // (fun_type, fun_name) // fun_type is ret type & expr types
    Call(FastExprType, Box<FastTypedExpr<'a>>, Vec<FastTypedExpr<'a>>), // (return_type, function_name_or_pointer, arguments)

    Input,
    RDInput,
    /* --- New to egg-eater --- */
    Null(FastExprType),
    Alloc(FastExprType),
    Update(
        FastExprType,
        Box<FastTypedExpr<'a>>,
        &'a str,
        Box<FastTypedExpr<'a>>,
    ),
    Lookup(FastExprType, Box<FastTypedExpr<'a>>, &'a str),
    /* --- New to bet --- */
    Unit,
    ArrayAlloc(FastExprType, Box<FastTypedExpr<'a>>), // array pointer: T
    ArrayLookup(FastExprType, Box<FastTypedExpr<'a>>, Box<FastTypedExpr<'a>>), // ArrayLookup(arr::array(t), ind::int)::t

    /// ArrayUpdate(arr::array(t), ind::int, elem::t)::t
    ArrayUpdate(
        FastExprType,
        Box<FastTypedExpr<'a>>,
        Box<FastTypedExpr<'a>>,
        Box<FastTypedExpr<'a>>,
    ),
    ArrayLen(FastExprType, Box<FastTypedExpr<'a>>), // ArrayLen(int, Box<array>)
}

#[derive(Debug, Clone)]
pub enum FastTypedFunction<'a> {
    // for use in a program
    Fun(&'a str, FastFunSignature<'a>, FastTypedExpr<'a>),
}

#[derive(Clone, Debug)]
pub enum FastStructLayout<'a> {
    Layout(HashMap<&'a str, i32>),
}

#[derive(Debug, Clone)]
pub enum FastTypedProg<'a> {
    // everything which will be compiled
    // structs aren't compiled-- they're reduced to sizes (alloc) and offsets (lookup, update)
    Program(
        FastExprType,
        TypeCheckState,
        HashMap<&'a str, FastStructSignature<'a>>,
        HashMap<&'a str, FastStructLayout<'a>>,
        Vec<FastTypedFunction<'a>>,
        FastTypedExpr<'a>,
    ),
}
