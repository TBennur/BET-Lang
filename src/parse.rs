use crate::semantics::*;
use crate::structs::*;
use sexp::Atom::*;
use sexp::*;

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        // atoms
        Sexp::Atom(I(n)) => match <i32>::try_from(*n) {
            Ok(num) => Expr::Number(num),
            Err(_) => panic!("Invalid"),
        },
        Sexp::Atom(S(id)) => match id.as_str() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            "input" => Expr::Input,
            id => Expr::Id(id_to_string(id)),
        },

        // vectors
        Sexp::List(vec) => match &vec[..] {
            // assume any vector whose first element isn't a keyword is a call
            [Sexp::Atom(S(fun_name)), args @ ..] if !is_keyword(fun_name) => Expr::Call(
                fun_name.to_string(),
                args.into_iter().map(parse_expr).collect(),
            ),
            // block
            // has the form block <expr>+
            [Sexp::Atom(S(op)), exprns @ ..] if op == "block" => {
                if exprns.len() == 0 {
                    panic!("Invalid")
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
                    panic!("Invalid")
                } else {
                    Expr::Let(
                        bindings
                            .into_iter()
                            .map(|element| match element {
                                Sexp::List(binding) => match &binding[..] {
                                    [Sexp::Atom(S(id)), e] => (id_to_string(id), parse_expr(e)),
                                    _ => panic!("Invalid"),
                                },
                                _ => panic!("Invalid"),
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
                    _ => panic!("Invalid"),
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
                    _ => panic!("Invalid"),
                },
                Box::new(parse_expr(e)),
            ),

            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn parse_defn(s: &Sexp) -> UserFunction {
    match s {
        /*
            <defn> := (
                fun
                <name>
                (
                    (<name> <type>)*
                )
                <type>
                <expr>
            )
        */
        Sexp::List(decl) => match &decl[..] {
            [Sexp::Atom(S(fun_keyword)), Sexp::Atom(S(fun_name)), Sexp::List(params), Sexp::Atom(S(ret_type)), fun_body]
                if fun_keyword == "fun" =>
            {
                let ret_type = type_str_to_expr_type(ret_type);

                let mut params_vec: Vec<(ExprType, String)> = Vec::new();
                for param_sexp in params {
                    match param_sexp {
                        Sexp::List(name_and_type) => match &name_and_type[..] {
                            [Sexp::Atom(S(name)), Sexp::Atom(S(param_type))] => {
                                params_vec
                                    .push((type_str_to_expr_type(param_type), name.to_string()));
                            }
                            _ => panic!("Invalid"),
                        },
                        _ => panic!("Invalid"),
                    }
                }

                UserFunction::UserFun(
                    fun_name.to_string(),
                    params_vec,
                    ret_type,
                    parse_expr(fun_body),
                )
            }

            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

pub fn parse_program(s: &Sexp) -> Prog {
    match s {
        Sexp::List(decl_and_body) => match decl_and_body.split_last() {
            None => panic!("Invalid"),

            Some((expr, defenitions)) => {
                // leave checking if function is defined for later
                Prog::Program(
                    defenitions.into_iter().map(parse_defn).collect(),
                    parse_expr(expr),
                )
            }
        },
        _ => panic!("Invalid"), // TODO: more specific panic?
    }
}
