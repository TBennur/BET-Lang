use crate::{
    consts::{ENTRYPOINT_LABEL, ERROR_LABEL, MAIN_LABEL},
    structs::*,
};
use once_cell::sync::Lazy;
use std::{
    collections::{HashMap, HashSet},
    sync::Mutex,
};

const NUM_LANG_TYPES: i32 = 3; // int, bool, unit
static STRUCT_COUNTER: Mutex<i32> = Mutex::new(NUM_LANG_TYPES);
/// Maps a struct enumeration number to the name of the corresponding struct type
///
/// Can contain invalid structs (struct names which weren't declared) which will be detected at type-check
pub static STRUCT_NUM_TO_NAME: Lazy<Mutex<HashMap<i32, String>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

/// Maps the name of a struct to the struct enumeration assigned to that struct
///
/// Can contain invalid structs (struct names which weren't declared) which will be detected at type-check
pub static STRUCT_NAME_TO_NUM: Lazy<Mutex<HashMap<String, i32>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

pub static UNOPS: Lazy<HashSet<&'static str>> =
    Lazy::new(|| HashSet::from(["print", "add1", "sub1", "!"]));

pub static STICKY_UNOPS: Lazy<HashSet<&'static str>> = Lazy::new(|| HashSet::from(["~"]));

pub static BET_BINOPS: Lazy<HashSet<&'static str>> =
    Lazy::new(|| HashSet::from(["==", ">=", "<=", "+", "-", "*", "<", ">", "||", "&&"]));

static KEYWORDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    HashSet::from([
        /* --- Types --- */
        "true",
        "false",
        "int",
        "bool",
        /* --- Declarations */
        "fun",
        /* --- Expressions --- */
        "input",
        "let",
        "set!",
        "if",
        "block",
        "repeat-until",
        /* Bin Ops */
        "+",
        "-",
        "*",
        "=",
        ">=",
        ">",
        "<=",
        "<",
        /* --- Un Ops --- */
        "print",
        "add1",
        "sub1",
        /* --- Reserved Labels --- */
        ENTRYPOINT_LABEL,
        MAIN_LABEL,
        ERROR_LABEL,
        /* --- New to eggeater --- */
        "struct", // struct *<name type>: used to declare a new struct type
        "null",   // e: ... | null: a universal pointer type, which can be a pointer to any type
        "new",    // (new <name>): evaluates to a pointer to a new struct of type <name>
        /* --- New to bet --- */
        "unit",
        "->",
        "new_arr", // new_arr(type, size)
    ])
});

pub fn is_keyword(id: &str) -> bool {
    KEYWORDS.contains(id)
}

/// Returns `.to_string()` argument isn't a reserved keyword (see `is_keyword`)
pub fn id_to_string(id: &str) -> String {
    return name_to_string(id, NameType::IdName);
}

pub fn id_to_str<'a>(id: &'a str) -> &'a str {
    name_to_str(id, NameType::IdName)
}

pub enum NameType {
    IdName,
    FunName,
    // FunParam,
    StructName,
    StructFieldName,
}

fn name_type_to_str(name_type: NameType) -> &'static str {
    match name_type {
        NameType::IdName => "Id",
        NameType::FunName => "Function Name",
        // NameType::FunParam => "Function Parameter",
        NameType::StructName => "Struct Name",
        NameType::StructFieldName => "Struct Field Name",
    }
}

/// Returns `.to_string()` of the name argument, if name isn't a reserved keyword
/// (see `is_keyword`).
///
/// Panics with a NameType-appropriate error message if name is a keyword.
pub fn name_to_string(name: &str, name_type: NameType) -> String {
    if is_keyword(name) {
        panic!(
            "Invalid: {} {} shares name with reserved keyword",
            name_type_to_str(name_type),
            name
        )
    }
    name.to_string()
}

pub fn name_to_str<'a>(name: &'a str, name_type: NameType) -> &'a str {
    if is_keyword(name) {
        panic!(
            "Invalid: {} {} shares name with reserved keyword",
            name_type_to_str(name_type),
            name
        )
    }
    name
}


/// Looks up a struct type enumeration and returns the name of the struct.
pub fn struct_type_enum_to_name(struct_type_enum: i32) -> Option<String> {
    let num_to_name_map = STRUCT_NUM_TO_NAME.lock().unwrap();
    let res = num_to_name_map.get(&struct_type_enum);
    match res {
        Some(s) => Some(s.to_string()),
        None => None,
    }
}

/// Gets type enumeration of name. Increments global counter & inserts if name doesn't yet have a type enumeration
pub fn struct_name_to_type_enum(name: &String) -> i32 {
    let mut name_num_map = STRUCT_NAME_TO_NUM.lock().unwrap();
    let mut num_name_map = STRUCT_NUM_TO_NAME.lock().unwrap();
    let mut counter = STRUCT_COUNTER.lock().unwrap();
    match name_num_map.get(name) {
        Some(&already_mapped) => already_mapped,
        None => {
            let our_num = *counter;
            *counter += 1;

            // insert into both name => i32 map and i32 => name map
            name_num_map.insert(name.to_string(), our_num);
            num_name_map.insert(our_num, name.to_string());

            // evaluate to our type enumeration
            our_num
        }
    }
}

pub fn single_type_str_to_expr_type(name: &str) -> ExprType {
    match &name as &str {
        "int" => ExprType::Int,
        "bool" => ExprType::Bool,
        "unit" => ExprType::Unit,

        // non basic type; assume is structname
        s if is_keyword(s) => panic!("Invalid type: reserved keyword{:?}", s),
        _ => ExprType::StructPointer(struct_name_to_type_enum(&name.to_string())), // TODO STRING
    }
}
