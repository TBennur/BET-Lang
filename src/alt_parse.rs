use crate::alt_stack::StepResult;
use crate::alt_stack_variants::{Constructor, SimpleStackState};
use crate::lex::{
    Atom::{F, I, S},
    Lexpr,
    Lexpr::{Atom, BraceList, CurlyList, List, ParenList},
};
use crate::parse_shared::{expect_block, parse_type};
use crate::semantics::*;
use crate::structs::*;
use core::panic;

type ParseState<'a> = SimpleStackState<Lexpr<'a>, FastExpr<'a>, FexprConstructor<'a>>;

enum FexprConstructor<'a> {
    Let(Vec<&'a str>),
    UnOp(Op1),
    BinOp(Op2),
    If,
    RepeatUntil,
    Set(&'a str),
    Block,
    Call(Option<&'a str>),
    Update(&'a str),
    Lookup(&'a str),
    ArrayLookup,
    ArrayUpdate,          // arr[ind] := new_val => ArrayUpdate(arr, ind, new_val)
    ArrayAlloc(ExprType), // new_arr(<type>, size) => ArrayAlloc(<type>, size)
    ArrayLen,             // arr_len(arr)
    NegUopp,
    WrappingParens,
}

impl<'a> Constructor<FastExpr<'a>> for FexprConstructor<'a> {
    fn construct(&mut self, parsed: &mut Vec<FastExpr<'a>>) -> FastExpr<'a> {
        match self {
            FexprConstructor::ArrayLookup => {
                if parsed.len() != 2 {
                    panic!("expected parsed length to have same length as unparsed")
                }
                let parsed_ind = parsed.pop().unwrap();
                let parsed_arr = parsed.pop().unwrap();
                FastExpr::ArrayLookup(Box::new(parsed_arr), Box::new(parsed_ind))
            }
            FexprConstructor::ArrayUpdate => {
                if parsed.len() != 3 {
                    panic!("expected parsed to have same length as unparsed")
                }
                let parsed_new_val = parsed.pop().unwrap();
                let parsed_index = parsed.pop().unwrap();
                let parsed_arr = parsed.pop().unwrap();

                FastExpr::ArrayUpdate(
                    Box::new(parsed_arr),
                    Box::new(parsed_index),
                    Box::new(parsed_new_val),
                )
            }
            FexprConstructor::ArrayAlloc(parsed_type) => {
                if parsed.len() != 1 {
                    panic!("expected parsed to have same len as unparsed")
                }

                let parsed_len = parsed.pop().unwrap();
                FastExpr::ArrayAlloc(std::mem::take(parsed_type), Box::new(parsed_len))
            }
            FexprConstructor::ArrayLen => {
                if parsed.len() != 1 {
                    panic!("Expected parsed to have the same length as unparsed")
                }
                FastExpr::ArrayLen(Box::new(parsed.pop().unwrap()))
            }
            FexprConstructor::If => {
                let arr: Result<[FastExpr<'a>; 3], _> = std::mem::take(parsed).try_into();
                match arr {
                    Ok(arr) => {
                        let [cond, if_true, if_false] = arr;
                        FastExpr::If(Box::new(cond), Box::new(if_true), Box::new(if_false))
                    }
                    Err(err) => unreachable!("invalid arg to constructor {:?}", err),
                }
            }
            FexprConstructor::Let(ids) => {
                let parsed_block = parsed.pop().unwrap();
                let parsed_bindings: Vec<_> = ids
                    .into_iter()
                    .map(|s| *s)
                    .zip(std::mem::take(parsed))
                    .collect();
                FastExpr::Let(parsed_bindings, Box::new(parsed_block))
            }
            FexprConstructor::NegUopp => {
                if parsed.len() != 1 {
                    unreachable!("we expect parsed to have equal length to unparsed, which is 1");
                }

                let parsed = parsed.pop().unwrap();
                match parsed {
                    FastExpr::Number(x) => FastExpr::Number(-x),
                    _ => unimplemented!(), // unary negation not yet implemented for anything other than string literals
                }
            }
            FexprConstructor::UnOp(uop_type) => {
                if parsed.len() != 1 {
                    unreachable!("we expect parsed to have the same length as unparded")
                };
                let parsed = parsed.pop().unwrap();

                FastExpr::UnOp(*uop_type, Box::new(parsed))
            }
            FexprConstructor::Call(name) => {
                let parsed_ptr = match name {
                    Some(checked_name) => FastExpr::FunName(*checked_name),
                    None => parsed.pop().unwrap(), // no name; pointer is last thing parsed
                };
                FastExpr::Call(Box::new(parsed_ptr), std::mem::take(parsed))
            }
            FexprConstructor::BinOp(binop) => {
                if parsed.len() == 2 {
                    let parsed_rhs = parsed.pop().unwrap();
                    let parsed_lhs = parsed.pop().unwrap();
                    FastExpr::BinOp(*binop, Box::new(parsed_lhs), Box::new(parsed_rhs))
                } else {
                    panic!("expected to get same number of parsed as unparsed")
                }
            }
            FexprConstructor::RepeatUntil => {
                if parsed.len() == 2 {
                    let parsed_cond = parsed.pop().unwrap();
                    let parsed_body = parsed.pop().unwrap();
                    FastExpr::RepeatUntil(Box::new(parsed_body), Box::new(parsed_cond))
                } else {
                    panic!("expected to get same number of parsed as unparsed")
                }
            }
            FexprConstructor::Set(id) => {
                if parsed.len() == 1 {
                    let parsed_new_val = parsed.pop().unwrap();
                    FastExpr::Set(*id, Box::new(parsed_new_val))
                } else {
                    panic!("expected to get same number of parsed as unparsed")
                }
            }
            FexprConstructor::Lookup(field_name) => {
                if parsed.len() == 1 {
                    let parsed_val = parsed.pop().unwrap();
                    FastExpr::Lookup(
                        Box::new(parsed_val),
                        name_to_str(field_name, NameType::StructFieldName),
                    )
                } else {
                    panic!("expected to get same number of parsed as unparsed")
                }
            }
            FexprConstructor::Update(field_name) => {
                if parsed.len() == 2 {
                    let parsed_new_val = parsed.pop().unwrap();
                    let parsed_struct_val = parsed.pop().unwrap();
                    FastExpr::Update(
                        Box::new(parsed_struct_val),
                        name_to_str(&field_name, NameType::StructFieldName),
                        Box::new(parsed_new_val),
                    )
                } else {
                    panic!("expected to get same number of parsed as unparsed")
                }
            }
            FexprConstructor::Block => FastExpr::Block(std::mem::take(parsed)),
            FexprConstructor::WrappingParens => {
                assert!(parsed.len() == 1);
                parsed.pop().unwrap()
            }
        }
    }
}

fn is_wrapped_expr_non_kword(lexpr: &Lexpr) -> bool {
    match lexpr {
        Atom(S(s)) => !is_keyword(s),
        CurlyList(_) => true,
        ParenList(_) => true,
        _ => false,
    }
}

fn package_lexr_vec<'a>(mut s: Vec<Lexpr<'a>>) -> Lexpr<'a> {
    if s.len() == 0 {
        unreachable!()
    } else if s.len() == 1 {
        s.pop().unwrap()
    } else {
        List(s)
    }
}

fn extract_binding_str<'a>(lexpr: Lexpr<'a>) -> Result<(&'a str, Lexpr), String> {
    let mut tokens = match lexpr {
        List(tokens) => tokens,
        _ => panic!("Bindings must be a List of tokens, {:?}", lexpr),
    };

    if tokens.len() < 3 {
        // need at least vec!["id", ":=", lexpr]
        panic!("Bad binding, {:?}", tokens)
    }

    let (id, walrus) = {
        let mut drained = tokens.drain(0..=1);
        let id = drained.next().unwrap();
        let walrus = drained.next().unwrap();
        (id, walrus)
        // drop drain
    };

    match (id, walrus) {
        (Atom(S(id)), Atom(S(walrus))) if walrus == ":=" => {
            Ok((id_to_str(id), package_lexr_vec(tokens)))
        }
        (id, walrus) => Err(format!(
            "Not a valid binding: {:?}, {:?}, {:?}",
            id, walrus, tokens
        )),
    }
}

impl<'a: 'b, 'b> crate::alt_stack::OneStep<(), ParseState<'a>> for Lexpr<'a> {
    fn step(self, _stack_state: &mut ()) -> StepResult<FastExpr<'a>, ParseState<'a>> {
        match self {
            // atoms
            Atom(I(n)) => match <i32>::try_from(n) {
                Ok(num) => StepResult::Terminal(FastExpr::Number(num)),
                Err(err) => panic!("Invalid: Could not parse number, raised {:?} instead", err),
            },
            Atom(S(id)) => {
                let expr = match id {
                    "true" => FastExpr::Boolean(true),
                    "false" => FastExpr::Boolean(false),
                    "input" => FastExpr::Input,
                    id => FastExpr::Id(id_to_str(id)), // panics if id is a keyword
                };
                StepResult::Terminal(expr)
            }
            Atom(F(_f)) => unimplemented!(),

            // List: a sequence of lexprs which are one expression
            List(mut list_contents) => match &mut *list_contents {
                // array lookup: arr[ind]
                [.., _, BraceList(_contents)] => {
                    let index = match list_contents.pop().unwrap() {
                        BraceList(mut contents) => {
                            if contents.len() == 1 {
                                contents.pop().unwrap()
                            } else {
                                panic!("Array index lookup with more than one index")
                            }
                        }
                        _ => unreachable!(),
                    };

                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![package_lexr_vec(list_contents), index],
                        FexprConstructor::ArrayLookup,
                    ))
                }

                // array update: arr[ind] := (new_val)
                [.., _, BraceList(_ind), Atom(S(walrus)), _new_val] if *walrus == ":=" => {
                    let new_val = list_contents.pop().unwrap();

                    let _walrus = list_contents.pop().unwrap();

                    let index = match list_contents.pop().unwrap() {
                        BraceList(mut contents) => {
                            if contents.len() == 1 {
                                contents.pop().unwrap()
                            } else {
                                panic!("Array index lookup with more than one index")
                            }
                        }
                        _ => unreachable!(),
                    };

                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![package_lexr_vec(list_contents), index, new_val],
                        FexprConstructor::ArrayUpdate,
                    ))
                }

                // array allocation: new_arr(<type>, len)
                [Atom(S(new_arr_kwd)), ParenList(_type_then_len)] if *new_arr_kwd == "new_arr" => {
                    let (unparsed_type, unparsed_len) = match list_contents.pop().unwrap() {
                        ParenList(mut type_then_len) => {
                            if type_then_len.len() != 2 {
                                panic!("Parse Error: Tried to parse new_arr but got wrong number of arguments (expected type, len, got: {:?})", type_then_len)
                            }
                            let unparsed_len = type_then_len.pop().unwrap();
                            let unparsed_type = type_then_len.pop().unwrap();
                            (unparsed_type, unparsed_len)
                        }
                        _ => unreachable!(),
                    };

                    let type_as_vec = match unparsed_type {
                        List(vec) => vec,
                        other => vec![other],
                    };

                    let parsed_type = parse_type(type_as_vec.as_slice());

                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![unparsed_len],
                        FexprConstructor::ArrayAlloc(parsed_type),
                    ))
                }

                [Atom(S(len_kwd)), ParenList(just_arr)] if *len_kwd == "arr_len" => {
                    if just_arr.len() != 1 {
                        panic!("arr_len keyword operates on a single array")
                    }
                    let unparsed_arr = just_arr.pop().unwrap();
                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![unparsed_arr],
                        FexprConstructor::ArrayLen,
                    ))
                }

                [Atom(S(if_kwd)), ParenList(cond_vec), true_block, Atom(S(else_kwd)), false_block]
                    if *if_kwd == "if" && *else_kwd == "else" =>
                {
                    // if conditions must have exactly one element
                    if cond_vec.len() != 1 {
                        panic!(
                            "Invalid: 'if' condition which doesn't have one expr: {:?}",
                            cond_vec
                        )
                    }

                    let cond = cond_vec.pop().unwrap();

                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![
                            cond,
                            std::mem::take(true_block),
                            std::mem::take(false_block),
                        ],
                        FexprConstructor::If,
                    ))
                }

                [Atom(S(keyword)), ParenList(bindings), scoped_block] if *keyword == "let" => {
                    let scoped_block = expect_block(std::mem::take(scoped_block)).unwrap(); // panic if not a block

                    if bindings.len() == 0 {
                        panic!("Invalid: Let binding with no bindings: {:?}", bindings)
                    }

                    let (ids, mut bound_values): (Vec<_>, Vec<_>) = bindings
                        .into_iter()
                        .map(|x| extract_binding_str(std::mem::take(x)).unwrap()) // validates binding structure
                        .unzip();

                    bound_values.push(scoped_block);

                    let new_context =
                        SimpleStackState::new(bound_values, FexprConstructor::Let(ids));
                    StepResult::Nonterminal(new_context)
                }

                // non-alphanum uops
                [Atom(S(uop)), receiver] if STICKY_UNOPS.contains(uop) => match *uop {
                    "~" => {
                        let receiver = std::mem::take(receiver);
                        StepResult::Nonterminal(SimpleStackState::new(
                            vec![receiver],
                            FexprConstructor::NegUopp,
                        ))
                    }
                    s => panic!("Invalid: Unknown sticky unary operation {:?}", s),
                },

                // alphanum unops
                [Atom(S(op1)), _, ..] if UNOPS.contains(op1) => {
                    // std::mem::take(op1); // now it shows as Lexpr::Stolen in list_contents
                    // let rest: Vec<_> = rest.iter_mut().map(|x| std::mem::take(x)).collect();
                    let uop_type = match *op1 {
                        "add1" => Op1::Add1,
                        "sub1" => Op1::Sub1,
                        "print" => Op1::Print,
                        "!" => Op1::Not,
                        s => panic!("Invalid: Unknown unary operation {:?}", s),
                    };

                    list_contents.remove(0); // drop op1
                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![package_lexr_vec(list_contents)],
                        FexprConstructor::UnOp(uop_type),
                    ))
                }

                // assume any vector in the form <wrapped_expr>(paren_list) is a function call
                [fn_name_or_ptr, ParenList(fun_args)]
                    if is_wrapped_expr_non_kword(fn_name_or_ptr) =>
                {
                    let mut fun_args = std::mem::take(fun_args);
                    let fn_name_or_ptr = std::mem::take(fn_name_or_ptr);

                    let fun_name = match &fn_name_or_ptr {
                        Atom(S(fun_name)) => Some(name_to_str(fun_name, NameType::FunName)),
                        _lexpr => None,
                    };

                    let unparsed = {
                        if let None = fun_name {
                            fun_args.push(fn_name_or_ptr); // need to parse fun pointer
                        }
                        fun_args
                    };

                    if unparsed.is_empty() {
                        StepResult::Terminal(FastExpr::Call(
                            Box::new(FastExpr::FunName(fun_name.unwrap())),
                            vec![],
                        ))
                    } else {
                        StepResult::Nonterminal(SimpleStackState::new(
                            unparsed,
                            FexprConstructor::Call(fun_name),
                        ))
                    }
                }

                // binops
                [lhs, Atom(S(op2)), rhs] if BET_BINOPS.contains(op2) => {
                    let binop = match *op2 {
                        "+" => Op2::Plus,
                        "-" => Op2::Minus,
                        "*" => Op2::Times,
                        "==" => Op2::Equal,
                        ">" => Op2::Greater,
                        ">=" => Op2::GreaterEqual,
                        "<" => Op2::Less,
                        "<=" => Op2::LessEqual,
                        "||" => Op2::Or,
                        "&&" => Op2::And,
                        s => panic!("Invalid: Unknown binary operation {:?}", s),
                    };
                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![std::mem::take(lhs), std::mem::take(rhs)],
                        FexprConstructor::BinOp(binop),
                    ))
                }

                // do {} while () loop
                [Atom(S(do_kwd)), body_block, Atom(S(while_kwd)), ParenList(cond)]
                    if *do_kwd == "do" && *while_kwd == "until" =>
                {
                    if cond.len() != 1 {
                        panic!(
                            "Got a cond for a do-while loop with length != 1: {:?}",
                            cond
                        )
                    }

                    let cond = cond.pop().unwrap();
                    let body_block = expect_block(std::mem::take(body_block)).unwrap();
                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![body_block, cond],
                        FexprConstructor::RepeatUntil,
                    ))
                }

                // set!
                // since this case is below the "let" case, we can match on rest
                [Atom(S(_id)), Atom(S(walrus)), _, ..] if *walrus == ":=" => {
                    // modify list_contents so that it contains only what we need to parse next
                    let id = {
                        let mut drain = list_contents.drain(0..=1);
                        let id = drain.next().unwrap();
                        drain.next().unwrap();
                        match id {
                            Atom(S(id)) => id,
                            _ => unreachable!(),
                        }
                    };

                    let id = id_to_str(id);
                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![package_lexr_vec(list_contents)],
                        FexprConstructor::Set(id),
                    ))
                }

                // null TODO SUPPORT ARR AND FUN PTRS
                [Atom(S(null_kwd)), Atom(S(struct_name))] if *null_kwd == "null" => {
                    StepResult::Terminal(FastExpr::Null(name_to_str(
                        &struct_name,
                        NameType::StructName,
                    )))
                }

                // new (alloc) TODO SUPPORT ARR AND FUN PTRS
                [Atom(S(new_kwd)), Atom(S(struct_name))] if *new_kwd == "new" => {
                    StepResult::Terminal(FastExpr::Alloc(name_to_str(
                        &struct_name,
                        NameType::StructName,
                    )))
                }

                // lookup: <some things>.field_name
                [.., _, Atom(S(dot)), Atom(S(_field_name))] if *dot == "." => {
                    let field_name = {
                        let field_name = match list_contents.pop().unwrap() {
                            Atom(S(field_name)) => field_name,
                            _ => unreachable!(),
                        }; // remove field name

                        let dot = match list_contents.pop().unwrap() {
                            Atom(S(dot)) => dot,
                            _ => unreachable!(),
                        }; // remove & drop dot
                        assert_eq!(dot, ".");

                        field_name // keep field_name
                    };

                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![package_lexr_vec(list_contents)],
                        FexprConstructor::Lookup(field_name),
                    ))
                }

                // update: <some things>.field_name := ()
                [.., _, Atom(S(dot)), Atom(S(_field_name)), Atom(S(walrus)), _new_val]
                    if *dot == "." && *walrus == ":=" =>
                {
                    // extract field_name, new_val, drop walrus, dot
                    let (field_name, new_val) = {
                        // drain contains (dot, field_name, walrus, new_val)
                        let mut drain = list_contents.drain(list_contents.len() - 4..);
                        match drain.next().unwrap() {
                            Atom(S(dot)) => assert_eq!(dot, "."),
                            _ => unreachable!(),
                        };
                        let field_name = match drain.next().unwrap() {
                            Atom(S(field_name)) => field_name,
                            _ => unreachable!(),
                        };
                        match drain.next().unwrap() {
                            Atom(S(walrus)) => assert_eq!(walrus, ":="),
                            _ => unreachable!(),
                        };
                        let new_val = drain.next().unwrap();
                        (field_name, new_val)
                    };

                    StepResult::Nonterminal(SimpleStackState::new(
                        vec![package_lexr_vec(list_contents), new_val],
                        FexprConstructor::Update(field_name),
                    ))
                }
                [] => StepResult::Terminal(FastExpr::Unit), // empty List is unit

                // TODO ARR LOOKUP AND UPDATE WITH []
                // - null check
                // - length check
                _ => panic!("Invalid list: {:#?}", list_contents),
            },

            // CurlyList: a block
            CurlyList(exprns) => {
                if exprns.len() == 0 {
                    StepResult::Terminal(FastExpr::Unit)
                } else {
                    StepResult::Nonterminal(SimpleStackState::new(exprns, FexprConstructor::Block))
                }
            }

            // ParenList: a sequence of lexprs which were wrapped in parens
            // at this point, we only expect there to be one element-- these parens only exist for lexing / are additional parens-- or none, which indicates unit
            ParenList(vec) => {
                match vec.len().cmp(&1) {
                    std::cmp::Ordering::Less => StepResult::Terminal(FastExpr::Unit), // zero
                    std::cmp::Ordering::Equal => StepResult::Nonterminal(SimpleStackState::new(
                        vec,
                        FexprConstructor::WrappingParens,
                    )), // TODO: TEST
                    std::cmp::Ordering::Greater => panic!("Invalid parens: {:?}.", vec),
                }
            }
            Lexpr::Stolen => unreachable!(), // stolen should be filtered by ParseState::new()

            Lexpr::BraceList(vec) => panic!(
                "invalid expression: brackets not used for array access [{:?}]",
                vec
            ),
        }
    }
}

pub fn alt_parse_expr<'a>(lexpr: Lexpr<'a>) -> FastExpr<'a> {
    let mut state = ();
    let mut stack: crate::alt_stack::IterativeStack<ParseState, ()> =
        crate::alt_stack::IterativeStack::new(&mut state);
    stack.iterate(lexpr)
}
