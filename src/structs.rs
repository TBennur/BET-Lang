
#[derive(Debug)]
pub enum Prog {
    Program(Vec<Function>, Expr),
}

#[derive(Debug)]
pub enum Function {
    SnekPrint,
    SnekError,
    UserFun(String, Vec<(ExprType, String)>, ExprType, Expr),
}

#[derive(Debug)]
pub enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
pub enum Reg {
    RAX,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Op1 {
    Add1,
    Sub1,
    Print,
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
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ExprType {
    Int,
    Bool,
}

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
    Input(ExprType),
    // Args(Vec<TypeExpr>)
}

pub fn extract_type(t: &TypedExpr) -> ExprType {
    match t {
        TypedExpr::Number(_) => ExprType::Int,
        TypedExpr::Boolean(_) => ExprType::Bool,
        TypedExpr::Id(expr_type, _) => *expr_type,
        TypedExpr::Let(expr_type, _, _) => *expr_type,
        TypedExpr::UnOp(expr_type, _, _) => *expr_type,
        TypedExpr::BinOp(expr_type, _, _, _) => *expr_type,
        TypedExpr::If(expr_type, _, _, _) => *expr_type,
        TypedExpr::RepeatUntil(expr_type, _, _) => *expr_type,
        TypedExpr::Set(expr_type, _, _) => *expr_type,
        TypedExpr::Block(expr_type, _) => *expr_type,
        TypedExpr::Input(expr_type) => *expr_type,
    }
}