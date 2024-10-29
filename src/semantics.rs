use crate::{
    consts::{ENTRYPOINT_LABEL, MAIN_LABEL, OVERFLOW_LABEL},
    structs::*,
};
use once_cell::sync::Lazy;
use std::collections::HashSet;

static KEYWORDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    HashSet::from([
        "fun",
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
        "print",
        "+",
        "-",
        "*",
        "=",
        ">=",
        ">",
        "<=",
        "<",
        "int",
        "bool",
        ENTRYPOINT_LABEL,
        MAIN_LABEL,
        OVERFLOW_LABEL,
    ])
});

pub fn is_keyword(id: &str) -> bool {
    KEYWORDS.contains(id)
}

pub fn id_to_string(id: &str) -> String {
    if is_keyword(id) {
        panic!("Invalid id {:?} is a keyword", id)
    }
    id.to_string()
}

pub fn type_str_to_expr_type(s: &String) -> ExprType {
    if s == "int" {
        ExprType::Int
    } else if s == "bool" {
        ExprType::Bool
    } else {
        panic!("Invalid type {:?}", s)
    }
}
