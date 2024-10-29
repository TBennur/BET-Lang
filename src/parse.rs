use core::panic;

use crate::semantics::*;
use crate::structs::*;
use sexp::Atom::*;
use sexp::*;

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        // atoms
        Sexp::Atom(I(n)) => match <i32>::try_from(*n) {
            Ok(num) => Expr::Number(num),
            Err(err) => panic!("Invalid: Could not parse number, raised {:?} instead", err),
        },
        Sexp::Atom(S(id)) => match id.as_str() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            "input" => Expr::Input,
            "null" => Expr::Null,
            id => Expr::Id(id_to_string(id)), // panics if id is a keyword
        },

        // vectors
        Sexp::List(vec) => match &vec[..] {
            // assume any vector whose first element isn't a keyword is a call
            [Sexp::Atom(S(fun_name)), args @ ..] if !is_keyword(fun_name) => Expr::Call(
                name_to_string(fun_name, NameType::FunName),
                args.into_iter().map(parse_expr).collect(),
            ),

            // alloc
            // has the form alloc <name>
            [Sexp::Atom(S(op)), Sexp::Atom(S(struct_name))] if op == "alloc" => {
                Expr::Alloc(name_to_string(struct_name, NameType::StructName))
            }

            // update
            // has the form update e1 <name> e2
            [Sexp::Atom(S(op)), e1, Sexp::Atom(S(field_name)), e2] if op == "update" => {
                Expr::Update(
                    Box::new(parse_expr(e1)),
                    name_to_string(field_name, NameType::StructFieldName),
                    Box::new(parse_expr(e2)),
                )
            }

            // lookup
            // has the form lookup e <name>
            [Sexp::Atom(S(op)), e, Sexp::Atom(S(field_name))] if op == "lookup" => Expr::Lookup(
                Box::new(parse_expr(e)),
                name_to_string(field_name, NameType::StructFieldName),
            ),

            // block
            // has the form block <expr>+
            [Sexp::Atom(S(op)), exprns @ ..] if op == "block" => {
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

            // set!
            // has the form set <name> <expr>
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), expr] if op == "set!" => {
                Expr::Set(id_to_string(name), Box::new(parse_expr(expr)))
            }

            // repeat-until
            // has the form repeat-until <expr> <expr>
            [Sexp::Atom(S(op)), body, stop_cond] if op == "repeat-until" => {
                Expr::RepeatUntil(Box::new(parse_expr(body)), Box::new(parse_expr(stop_cond)))
            }

            // if expression
            // has the form if <expr> <expr> <expr>
            [Sexp::Atom(S(op)), cond, then_case, else_case] if op == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(then_case)),
                Box::new(parse_expr(else_case)),
            ),

            // let expression
            // has the form let ((binding1), (binding2)) (in expression)
            [Sexp::Atom(S(op)), Sexp::List(bindings), finally] if op == "let" => {
                if bindings.len() == 0 {
                    panic!("Invalid: Empty block")
                } else {
                    Expr::Let(
                        bindings
                            .into_iter()
                            .map(|element| match element {
                                Sexp::List(binding) => match &binding[..] {
                                    [Sexp::Atom(S(id)), e] => (id_to_string(id), parse_expr(e)),
                                    s => panic!("Invalid: Improperly formed let binding {:?}", s),
                                },
                                s => panic!("Invalid: Improperly formed let expression {:?}", s),
                            })
                            .collect(),
                        Box::new(parse_expr(finally)),
                    )
                }
            }

            // match bin & un ops after everything else, so we don't try to
            // match other "un" ops like, ex, set

            // bin ops (exactly three things in vec)
            [Sexp::Atom(S(op)), arg1, arg2] => Expr::BinOp(
                match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    "=" => Op2::Equal,
                    ">" => Op2::Greater,
                    ">=" => Op2::GreaterEqual,
                    "<" => Op2::Less,
                    "<=" => Op2::LessEqual,
                    s => panic!("Invalid: Unknown binary operation {:?}", s),
                },
                Box::new(parse_expr(arg1)),
                Box::new(parse_expr(arg2)),
            ),

            // un ops (exactly two things in vec)
            [Sexp::Atom(S(op)), e] => Expr::UnOp(
                match op.as_str() {
                    "add1" => Op1::Add1,
                    "sub1" => Op1::Sub1,
                    "print" => Op1::Print,
                    s => panic!("Invalid: Unknown unary operation {:?}", s),
                },
                Box::new(parse_expr(e)),
            ),

            s => panic!("Invalid: Unknown vector expression {:?}", s),
        },
        s => panic!("Invalid: Unknown atomic expression {:?}", s),
    }
}

fn parse_struct_def(s: &Sexp) -> Option<UserStruct> {
    // check if it could even be a struct defenition
    let decl = match s {
        Sexp::List(declaration) => declaration,
        _ => return None,
    };

    let (struct_name, fields) = match &decl[..] {
        [Sexp::Atom(S(s_keyword)), Sexp::Atom(S(s_name)), Sexp::List(_fields)]
            if s_keyword == "struct" =>
        {
            (s_name, _fields)
        }
        _ => return None,
    };

    // from here on, we know it must be a struct declaration; so panics instead of None

    // create a unique type enumeration for the struct, if it doesn't yet exist
    type_str_to_expr_type(struct_name);

    let mut fields_vec = Vec::new();
    for f in fields {
        let name_and_type = match f {
            Sexp::List(_name_and_type) => _name_and_type,
            _ => panic!("Invalid: Malformed struct field {:?}", f),
        };

        let tup = match &name_and_type[..] {
            [Sexp::Atom(S(field_name)), Sexp::Atom(S(field_type))] => (
                type_str_to_expr_type(field_type),
                name_to_string(&field_name, NameType::StructFieldName), // panics if keyword
            ),
            _ => panic!("Invalid: Malformed struct field binding {:?}", f),
        };
        fields_vec.push(tup);
    }

    Some(UserStruct::UserStruct(
        // checks struct name isn't a reserved keyword
        name_to_string(&struct_name, NameType::StructName),
        fields_vec,
    ))
}

fn parse_fn_def(s: &Sexp) -> Option<UserFunction> {
    // check if it could even be a function defenition
    let decl = match s {
        Sexp::List(declaration) => declaration,
        _ => return None,
    };

    let (fun_name, params, ret_type, fun_body) = match &decl[..] {
        [Sexp::Atom(S(fun_keyword)), Sexp::Atom(S(fun_name)), Sexp::List(params), Sexp::Atom(S(ret_type)), fun_body]
            if fun_keyword == "fun" =>
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
        .map(|param| {
            let name_and_type = match param {
                Sexp::List(_name_and_type) => _name_and_type,
                _ => panic!("Invalid: Improperly formed parameter definition {:?}", s),
            };

            match &name_and_type[..] {
                [Sexp::Atom(S(name)), Sexp::Atom(S(param_type))] => (
                    type_str_to_expr_type(param_type),
                    name_to_string(name, NameType::FunParam),
                ),
                _ => panic!("Invalid: Improperly formed parameter binding {:?}", s),
            }
        })
        .collect();

    Some(UserFunction::UserFun(
        fun_name,
        FunSignature::Sig(ret_type, params_vec),
        parse_expr(fun_body),
    ))
}

pub fn parse_program(s: &Sexp) -> Prog {
    let decl_and_body = match s {
        Sexp::List(decl_and_body) => decl_and_body,
        _ => panic!("Invalid: Program is an atom expression {:?}", s),
    };

    let (expr, defenitions) = match decl_and_body.split_last() {
        Some((expr, defenitions)) => (expr, defenitions),
        None => panic!("Invalid: Malformed program"),
    };

    let mut fn_defs = Vec::new();
    let mut struct_defs = Vec::new();

    // check whether function, struct, or error
    for defenition in defenitions {
        if let Some(parsed) = parse_fn_def(defenition) {
            // must be a function
            fn_defs.push(parsed);
            continue;
        }

        if let Some(parsed) = parse_struct_def(defenition) {
            // must be a struct
            struct_defs.push(parsed);
            continue;
        }

        // must be illegal!
        panic!("Invalid: Improperly formed struct binding {:?}", s)
    }

    Prog::Program(struct_defs, fn_defs, parse_expr(expr))
}
