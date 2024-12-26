use crate::alt_parse::alt_parse_expr;
use crate::lex;
use crate::parse::parse_expr_old;
use crate::semantics::*;
use crate::structs::*;
use core::panic;
use lex::Atom::S;
use lex::Lexpr;
use lex::Lexpr::{Atom, BraceList, CurlyList, List, ParenList};

fn parse_type_annotated_name_str<'a>(
    defn: &Lexpr,
    name_and_type: &'a Lexpr,
) -> (ExprType, &'a str) {
    let name_and_type = match name_and_type {
        List(_name_and_type) => _name_and_type,
        _ => panic!(
            "Invalid: Improperly formed type annotation {:?} in {:?}",
            name_and_type, defn
        ),
    };

    match &name_and_type[..] {
        // single type
        [Atom(S(field_name)), Atom(S(type_anotation)), field_type @ ..]
            if *type_anotation == "::" =>
        {
            // Will check field isn't a duplicate in type-check
            (
                parse_type(field_type),
                name_to_str(field_name, NameType::StructFieldName), // panics if keyword
            )
        }

        _ => panic!(
            "Invalid: Improperly formed type annotation {:?} in {:?}",
            name_and_type, defn
        ),
    }
}

fn parse_struct_def<'a>(s: &'a Lexpr<'a>) -> Option<FastUserStruct<'a>> {
    let decl = match s {
        List(declaration) => declaration,
        _ => return None,
    };

    let (struct_name, fields) = match &decl[..] {
        [Atom(S(defn_type)), Atom(S(struct_name)), ParenList(fields)] if *defn_type == "struct" => {
            (struct_name, fields)
        }
        _ => return None,
    };

    // from here on, we know it must be a struct declaration; so panics instead of None

    // create a unique type enumeration for the struct, if it doesn't yet exist, panic if keyword
    single_type_str_to_expr_type(struct_name);

    let mut fields_vec = Vec::new();
    for f in fields {
        fields_vec.push(parse_type_annotated_name_str(s, f));
    }

    if fields_vec.is_empty() {
        panic!("Invalid: Struct declaration with no fields: {:?}", s)
    }

    Some(FastUserStruct::UserStruct(
        // checks struct name isn't a reserved keyword
        name_to_str(&struct_name, NameType::StructName),
        FastStructSignature::Sig(fields_vec),
    ))
}

fn parse_fn_def<'a>(s: &'a Lexpr<'a>, v: Variant) -> Option<FastUserFunction<'a>> {
    // check if it could even be a function definition
    let decl = match s {
        List(declaration) => declaration,
        _ => return None,
    };

    let (fun_name, params, ret_type, fun_body) = match &decl[..] {
        [Atom(S(fun_keyword)), Atom(S(fun_name)), ParenList(params), Atom(S(type_annotation_kword)), ret_type @ .., fun_body]
            if *fun_keyword == "fun" && *type_annotation_kword == "::" =>
        {
            (fun_name, params, ret_type, fun_body)
        }
        _ => return None,
    };

    // get return type
    let ret_type = parse_type(ret_type);

    // from here on, we know it must be a function declaration; so panics instead of None

    // check name (panics if keyword)
    let fun_name = name_to_str(fun_name, NameType::FunName);

    // parse params
    let params_vec = params
        .into_iter()
        .map(|param| parse_type_annotated_name_str(s, param))
        .collect();

    Some(FastUserFunction::UserFun(
        fun_name,
        FastFunSignature::Sig(ret_type, params_vec),
        parse_expr(expect_block(fun_body.clone()).unwrap(), v), // TODO: CLONE
    ))
}

pub fn parse_prog<'a>(lexpr: &'a mut Lexpr<'a>, v: Variant) -> FastProg<'a> {
    let decl_and_body = match lexpr {
        Lexpr::List(decl_and_body) => decl_and_body,
        _ => panic!("Invalid: Program is an atom expression {:?}", lexpr),
    };

    let (body, definitions) = match decl_and_body.split_last_mut() {
        Some((body, definitions)) => (body, definitions),
        None => panic!("Invalid: Malformed program"),
    };

    let mut fn_defs = Vec::new();
    let mut struct_defs = Vec::new();

    // check whether function, struct, or error
    // Does not check whether referenced functions or structs exists (that's in typechecking)
    for definition in definitions {
        if let Some(parsed) = parse_fn_def(definition, v) {
            // must be a function
            fn_defs.push(parsed);
            continue;
        }

        if let Some(parsed) = parse_struct_def(definition) {
            // must be a struct
            struct_defs.push(parsed);
            continue;
        }

        // must be illegal!
        panic!("Invalid: Improperly formed definition {:#?}", definition)
    }
    FastProg::Program(struct_defs, fn_defs, parse_expr(std::mem::take(body), v)) // TODO: CLONE
}

/// Parses a slice of Lexrp into an ExprType
///
/// Handles:
/// - base types: `unit`, `int`, `bool`
/// - structs
/// - function pointers
pub fn parse_type(type_annotation: &[Lexpr]) -> ExprType {
    match type_annotation {
        // struct, base type
        [Atom(S(single_type))] => single_type_str_to_expr_type(single_type),

        // array type
        [BraceList(elem_type)] => {
            if elem_type.len() != 1 {
                panic!("array type should only have one type: {:?}", elem_type)
            }
            ExprType::Array(Box::new(parse_type(&elem_type[0..1])))
        }

        // function type
        [ParenList(arg_types), Atom(S(arrow)), ret_type @ ..] if *arrow == "->" => {
            let parsed_arg_types = arg_types
                .into_iter()
                .map(|lexpr| match lexpr {
                    List(vec) => &vec[..],
                    a => std::slice::from_ref(a),
                })
                .map(parse_type)
                .collect();
            let parsed_ret_type: ExprType = parse_type(ret_type);
            ExprType::FunctionPointer(parsed_arg_types, Box::new(parsed_ret_type))
        }

        _ => panic!("Invalid type annotation: {:?}", type_annotation),
    }
}

pub fn expect_block(lexpr: Lexpr) -> Result<Lexpr, String> {
    if let CurlyList(_stuff) = lexpr {
        Ok(CurlyList(_stuff))
    } else {
        Err(format!(
            "Tried to parse a block, but didn't find curly braces: {:?}",
            lexpr
        ))
    }
}

#[derive(Clone, Copy)]
pub enum Variant {
    Old,
    Alt,
}

fn parse_expr<'a>(lexpr: Lexpr<'a>, v: Variant) -> FastExpr<'a> {
    match v {
        Variant::Old => parse_expr_old(lexpr),
        Variant::Alt => alt_parse_expr(lexpr),
    }
}
