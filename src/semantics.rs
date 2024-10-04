use crate::{consts::{ENTRYPOINT_LABEL, MAIN_LABEL}, structs::*};

pub fn is_keyword(id: &str) -> bool {
    return vec![
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
        MAIN_LABEL
    ]
    .contains(&id);
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
