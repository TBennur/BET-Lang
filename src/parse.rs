use core::panic;

use crate::lex;
use crate::semantics::*;
use crate::structs::*;

use lex::Atom::{F, I, S};
use lex::Lexpr;
use lex::Lexpr::{Atom, CurlyList, List, ParenList};

fn unwrap_lexpr_list<'a>(s: &[Lexpr]) -> Lexpr {
    let owned = if s.len() == 0 {
        unreachable!()
    } else if s.len() == 1 {
        s[0].to_owned()
    } else {
        List(s.to_vec())
    };
    owned
}

fn parse_binding(lexpr: &Lexpr) -> Result<(String, Expr), String> {
    let tokens = match lexpr {
        List(tokens) => tokens,
        _ => panic!("Bindings must be a List of tokens, {:?}", lexpr),
    };

    match &tokens[..] {
        [Atom(S(id)), Atom(S(walrus)), rest @ ..] if walrus == ":=" => {
            Ok((id_to_string(id), parse_expr(&unwrap_lexpr_list(rest))))
        }
        _ => Err(format!("Not a valid binding: {:?}", lexpr)),
    }
}

fn parse_expr(lexpr: &Lexpr) -> Expr {
    match lexpr {
        // atoms
        Atom(I(n)) => match <i32>::try_from(*n) {
            Ok(num) => Expr::Number(num),
            Err(err) => panic!("Invalid: Could not parse number, raised {:?} instead", err),
        },
        Atom(S(id)) => match id.as_str() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            "input" => Expr::Input,
            id => Expr::Id(id_to_string(id)), // panics if id is a keyword
        },
        Atom(F(_f)) => unimplemented!(),

        // List: a sequence of lexprs which are one expression
        List(vec) => match &vec[..] {
            [Atom(S(if_kwd)), ParenList(cond_vec), true_block, Atom(S(else_kwd)), false_block]
                if if_kwd == "if" && else_kwd == "else" =>
            {
                // if conditions must have exactly one element
                let cond = match &cond_vec[..] {
                    [_cond] => _cond,
                    _ => panic!(
                        "Invalid: 'if' condition which doesn't have one expr: {:?}",
                        vec
                    ),
                };

                Expr::If(
                    Box::new(parse_expr(cond)),
                    Box::new(parse_block(true_block).unwrap()),
                    Box::new(parse_block(false_block).unwrap()),
                )
            }

            [Atom(S(keyword)), ParenList(bindings), scoped_block] if keyword == "let" => {
                if bindings.len() == 0 {
                    panic!("Invalid: Let binding with no bindings: {:?}", vec)
                }

                // parse each binding in the scoped block
                Expr::Let(
                    bindings
                        .into_iter()
                        .map(|binding| parse_binding(binding).unwrap())
                        .collect(),
                    Box::new(parse_block(scoped_block).unwrap()),
                )
            }

            // non-alphanum uops
            [Atom(S(uop)), receiver] if STICKY_UNOPS.contains(&uop.as_str()) => {
                match uop.as_str() {
                    "~" => match parse_expr(receiver) {
                        Expr::Number(x) => Expr::Number(-x),
                        _ => unimplemented!(), // unary negation not yet implemented for anything other than string literals
                    },
                    s => panic!("Invalid: Unknown sticky unary operation {:?}", s),
                }
            }

            // alphanum unops
            [Atom(S(op1)), rest @ ..] if UNOPS.contains(&op1.as_str()) => Expr::UnOp(
                match op1.as_str() {
                    "add1" => Op1::Add1,
                    "sub1" => Op1::Sub1,
                    "print" => Op1::Print,
                    "!" => Op1::Not,
                    s => panic!("Invalid: Unknown unary operation {:?}", s),
                },
                Box::new(parse_expr(&unwrap_lexpr_list(rest))),
            ),

            // assume any vector whose first element isn't a keyword is a call
            [Atom(S(fun_name)), ParenList(fun_args)] if !is_keyword(fun_name) => {
                Expr::Call(
                    name_to_string(fun_name, NameType::FunName),
                    fun_args.into_iter().map(parse_expr).collect(),
                )
            },

            // binops
            [lhs, Atom(S(op2)), rhs] if BET_BINOPS.contains(&op2.as_str()) => Expr::BinOp(
                match op2.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    "==" => Op2::Equal,
                    ">" => Op2::Greater,
                    ">=" => Op2::GreaterEqual,
                    "<" => Op2::Less,
                    "<=" => Op2::LessEqual,
                    "||" => Op2::Or,
                    s => panic!("Invalid: Unknown binary operation {:?}", s),
                },
                Box::new(parse_expr(lhs)),
                Box::new(parse_expr(rhs)),
            ),

            // do {} while () loop
            [Atom(S(do_kwd)), body_block, Atom(S(while_kwd)), ParenList(cond)]
                if do_kwd == "do" && while_kwd == "until" =>
            {
                let cond = match &cond[..] {
                    [one] => one,
                    _ => panic!("Got a cond for a do-while loop with length != 1: {:?}", vec),
                };

                Expr::RepeatUntil(
                    Box::new(parse_block(body_block).unwrap()),
                    Box::new(parse_expr(cond)),
                )
            }

            // set!
            // since this case is below the "let" case, we can match on rest
            [Atom(S(id)), Atom(S(walrus)), rest @ ..] if walrus == ":=" => Expr::Set(
                id_to_string(id),
                Box::new(parse_expr(&unwrap_lexpr_list(rest))),
            ),

            // null
            [Atom(S(null_kwd)), Atom(S(struct_name))] if null_kwd == "null" => {
                Expr::Null(name_to_string(struct_name, NameType::StructName))
            }

            // new (alloc)
            [Atom(S(new_kwd)), Atom(S(struct_name))] if new_kwd == "new" => {
                Expr::Alloc(name_to_string(struct_name, NameType::StructName))
            }

            // lookup: <some things>.field_name
            [first @ .., Atom(S(dot)), Atom(S(field_name))] if dot == "." => Expr::Lookup(
                Box::new(parse_expr(&unwrap_lexpr_list(first))),
                name_to_string(field_name, NameType::StructFieldName),
            ),

            // update: <some things>.field_name := ()
            [first @ .., Atom(S(dot)), Atom(S(field_name)), Atom(S(walrus)), new_val]
                if dot == "." && walrus == ":=" =>
            {
                Expr::Update(
                    Box::new(parse_expr(&unwrap_lexpr_list(first))),
                    name_to_string(field_name, NameType::StructFieldName),
                    Box::new(parse_expr(new_val)),
                )
            }

            _ => panic!("Invalid list: {:#?}", vec),
        },

        // CurlyList: a block
        CurlyList(exprns) => {
            if exprns.len() == 0 {
                panic!("Invalid: Empty block")
            } else {
                Expr::Block(
                    exprns
                        .into_iter()
                        .map(|element| parse_expr(element))
                        .collect(),
                )
            }
        }

        // ParenList: a sequence of lexprs which were wrapped in parens
        // at this point, we only expect there to be one element-- these parens only exist for lexing / are additional parens
        ParenList(vec) => {
            if let [one] = &vec[..] {
                parse_expr(one)
            } else {
                panic!("Invalid wrapping parens: {:?}.", vec)
            }
        }
    }
}

fn parse_block(lexpr: &Lexpr) -> Result<Expr, String> {
    if let CurlyList(_stuff) = lexpr {
        Ok(parse_expr(lexpr))
    } else {
        Err(format!(
            "Tried to parse a block, but didn't find curly braces: {:?}",
            lexpr
        ))
    }
}

fn parse_type_annotated_name(defn: &Lexpr, name_and_type: &Lexpr) -> (ExprType, String) {
    let name_and_type = match name_and_type {
        List(_name_and_type) => _name_and_type,
        _ => panic!(
            "Invalid: Improperly formed type annotation {:?} in {:?}",
            name_and_type, defn
        ),
    };

    match &name_and_type[..] {
        [Atom(S(field_name)), Atom(S(type_anotation)), Atom(S(field_type))]
            if type_anotation == "::" =>
        {
            // Will check field isn't a duplicate in type-check
            (
                type_str_to_expr_type(field_type),
                name_to_string(&field_name, NameType::StructFieldName), // panics if keyword
            )
        }
        _ => panic!(
            "Invalid: Improperly formed type annotation {:?} in {:?}",
            name_and_type, defn
        ),
    }
}

fn parse_struct_def(s: &Lexpr) -> Option<UserStruct> {
    let decl = match s {
        List(declaration) => declaration,
        _ => return None,
    };

    let (struct_name, fields) = match &decl[..] {
        [Atom(S(defn_type)), Atom(S(struct_name)), ParenList(fields)] if defn_type == "struct" => {
            (struct_name, fields)
        }
        _ => return None,
    };

    // from here on, we know it must be a struct declaration; so panics instead of None

    // create a unique type enumeration for the struct, if it doesn't yet exist
    type_str_to_expr_type(struct_name);

    let mut fields_vec = Vec::new();
    for f in fields {
        fields_vec.push(parse_type_annotated_name(s, f));
    }

    if fields_vec.is_empty() {
        panic!("Invalid: Struct declaration with no fields: {:?}", s)
    }

    Some(UserStruct::UserStruct(
        // checks struct name isn't a reserved keyword
        name_to_string(&struct_name, NameType::StructName),
        StructSignature::Sig(fields_vec),
    ))
}

fn parse_fn_def(s: &Lexpr) -> Option<UserFunction> {
    // check if it could even be a function defenition
    let decl = match s {
        List(declaration) => declaration,
        _ => return None,
    };

    let (fun_name, params, ret_type, fun_body) = match &decl[..] {
        [Atom(S(fun_keyword)), Atom(S(fun_name)), ParenList(params), Atom(S(type_annotation_kword)), Atom(S(ret_type)), fun_body]
            if fun_keyword == "fun" && type_annotation_kword == "::" =>
        {
            (fun_name, params, ret_type, fun_body)
        }
        _ => return None,
    };

    // from here on, we know it must be a function declaration; so panics instead of None

    // check name (panics if keyword)
    let fun_name = name_to_string(fun_name, NameType::FunName);

    // get return type
    let ret_type = type_str_to_expr_type(ret_type);

    // parse params
    let params_vec = params
        .into_iter()
        .map(|param| parse_type_annotated_name(s, param))
        .collect();

    Some(UserFunction::UserFun(
        fun_name,
        FunSignature::Sig(ret_type, params_vec),
        parse_block(fun_body).unwrap(),
    ))
}

pub fn parse_program(lexpr: &Lexpr) -> Prog {
    let decl_and_body = match lexpr {
        Lexpr::List(decl_and_body) => decl_and_body,
        _ => panic!("Invalid: Program is an atom expression {:?}", lexpr),
    };

    let (body, definitions) = match decl_and_body.split_last() {
        Some((body, definitions)) => (body, definitions),
        None => panic!("Invalid: Malformed program"),
    };

    let mut fn_defs = Vec::new();
    let mut struct_defs = Vec::new();

    // check whether function, struct, or error
    // Does not check whether referenced functions or structs exists (that's in typechecking)
    for definition in definitions {
        if let Some(parsed) = parse_fn_def(definition) {
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
        panic!("Invalid: Improperly formed defenition {:#?}", definition)
    }

    Prog::Program(struct_defs, fn_defs, parse_expr(body))
}
