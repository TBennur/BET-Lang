use crate::lex;
use crate::semantics::*;
use crate::structs::*;
use core::panic;
use lex::Atom::{F, I, S};
use lex::Lexpr;
use lex::Lexpr::{Atom, CurlyList, List, ParenList};

fn package_lexr_vec(mut s: Vec<Lexpr>) -> Lexpr {
    if s.len() == 0 {
        unreachable!()
    } else if s.len() == 1 {
        s.pop().unwrap()
    } else {
        List(s)
    }
}

fn extract_binding(lexpr: Lexpr) -> Result<(String, Lexpr), String> {
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
            Ok((id_to_string(&id), package_lexr_vec(tokens)))
        }
        (id, walrus) => Err(format!(
            "Not a valid binding: {:?}, {:?}, {:?}",
            id, walrus, tokens
        )),
    }
}

enum ParseRetval {
    Done(Expr),
    KeepGoing(Lexpr),
}

fn construct_if(subexprs: Vec<Expr>) -> Expr {
    let arr: Result<[Expr; 3], _> = subexprs.try_into();
    match arr {
        Ok(arr) => {
            let [cond, if_true, if_false] = arr;
            Expr::If(Box::new(cond), Box::new(if_true), Box::new(if_false))
        }
        Err(err) => unreachable!("invalid arg to constructor {:?}", err),
    }
}

fn bad(_exprs: Vec<Expr>) -> Expr {
    panic!("constructor already consumed")
}

struct ParserState {
    unparsed: Vec<Lexpr>,
    parsed: Vec<Expr>,
    constructor: Box<dyn FnOnce(Vec<Expr>) -> Expr>,
}

impl ParserState {
    fn new(
        mut unparsed: Vec<Lexpr>,
        constructor: impl FnOnce(Vec<Expr>) -> Expr + 'static,
    ) -> ParserState {
        unparsed.reverse(); // so that popping still scans Left to Right

        if unparsed.is_empty() {
            panic!("expected non-zero parsing to do")
        }

        ParserState {
            unparsed,
            parsed: Vec::new(),
            constructor: Box::new(constructor),
        }
    }

    /// Consume an expression; None if not enough subexpressions to construct, Some if constructed Expr
    fn consume(&mut self, expr: Expr) -> ParseRetval {
        self.parsed.push(expr);

        match self.unparsed.pop() {
            None => {
                let constructor = std::mem::replace(&mut self.constructor, Box::new(bad));
                ParseRetval::Done(constructor(std::mem::take(&mut self.parsed)))
            }
            Some(parse_next) => ParseRetval::KeepGoing(parse_next),
        }
    }

    fn get_next(&mut self) -> ParseRetval {
        ParseRetval::KeepGoing(self.unparsed.pop().unwrap())
    }
}

pub struct IterativeParser {
    stack: Vec<ParserState>,
}

fn neg_unop(mut parsed: Vec<Expr>) -> Expr {
    if parsed.len() != 1 {
        unreachable!("we expect parsed to have equal length to unparsed, which is 1");
    }

    let parsed = parsed.pop().unwrap();
    match parsed {
        Expr::Number(x) => Expr::Number(-x),
        _ => unimplemented!(), // unary negation not yet implemented for anything other than string literals
    }
}

impl IterativeParser {
    pub fn new() -> IterativeParser {
        IterativeParser { stack: Vec::new() }
    }

    fn push_state(&mut self, mut to_push: ParserState) -> ParseRetval {
        let parse_next = to_push.get_next();
        self.stack.push(to_push);
        parse_next
    }

    /// Consume an expression; None if not done parsing, or Some if done
    fn consume_expr(&mut self, expr: Expr) -> ParseRetval {
        let mut to_consume = expr;
        loop {
            let mut curr_context: ParserState = match self.stack.pop() {
                None => return ParseRetval::Done(to_consume), // this isn't a subexpression, it's the final expression
                Some(state) => state,
            };

            let context_sub_res = curr_context.consume(to_consume);
            match context_sub_res {
                ParseRetval::Done(consume_next) => {
                    to_consume = consume_next;
                }
                ParseRetval::KeepGoing(parse_next) => {
                    self.stack.push(curr_context); // this expression needs more, so is still current context
                    return ParseRetval::KeepGoing(parse_next);
                }
            }
        }
    }

    pub fn parse_lexpr(&mut self, lexpr: Lexpr) -> Expr {
        let mut currently_parsing = lexpr;
        loop {
            let parse_subres = match currently_parsing {
                // atoms
                Atom(I(n)) => match <i32>::try_from(n) {
                    Ok(num) => self.consume_expr(Expr::Number(num)),
                    Err(err) => panic!("Invalid: Could not parse number, raised {:?} instead", err),
                },
                Atom(S(ref id)) => {
                    let expr = match id.as_str() {
                        "true" => Expr::Boolean(true),
                        "false" => Expr::Boolean(false),
                        "input" => Expr::Input,
                        id => Expr::Id(id_to_string(id)), // panics if id is a keyword
                    };
                    self.consume_expr(expr)
                }
                Atom(F(_f)) => unimplemented!(),

                // List: a sequence of lexprs which are one expression
                List(mut list_contents) => match &mut *list_contents {
                    [Atom(S(if_kwd)), ParenList(cond_vec), true_block, Atom(S(else_kwd)), false_block]
                        if if_kwd == "if" && else_kwd == "else" =>
                    {
                        // if conditions must have exactly one element
                        if cond_vec.len() != 1 {
                            panic!(
                                "Invalid: 'if' condition which doesn't have one expr: {:?}",
                                cond_vec
                            )
                        }

                        let cond = cond_vec.pop().unwrap();

                        let new_context = ParserState::new(
                            vec![
                                cond,
                                std::mem::take(true_block),
                                std::mem::take(false_block),
                            ],
                            construct_if,
                        );
                        self.push_state(new_context)
                    }

                    [Atom(S(keyword)), ParenList(bindings), scoped_block] if keyword == "let" => {
                        let scoped_block = expect_block(std::mem::take(scoped_block)).unwrap(); // panic if not a block

                        if bindings.len() == 0 {
                            panic!("Invalid: Let binding with no bindings: {:?}", bindings)
                        }

                        let (ids, mut bound_values): (Vec<_>, Vec<_>) = bindings
                            .into_iter()
                            .map(|x| extract_binding(std::mem::take(x)).unwrap()) // validates binding structure
                            .unzip();

                        bound_values.push(scoped_block);

                        let new_context = ParserState::new(bound_values, |mut parsed| {
                            let parsed_block = parsed.pop().unwrap();
                            let parsed_bindings: Vec<_> = ids.into_iter().zip(parsed).collect();
                            Expr::Let(parsed_bindings, Box::new(parsed_block))
                        });

                        self.push_state(new_context)
                    }

                    // non-alphanum uops
                    [Atom(S(uop)), receiver] if STICKY_UNOPS.contains(&uop.as_str()) => {
                        match uop.as_str() {
                            "~" => {
                                let receiver = std::mem::take(receiver);
                                let new_context = ParserState::new(vec![receiver], neg_unop);
                                self.push_state(new_context)
                            }
                            s => panic!("Invalid: Unknown sticky unary operation {:?}", s),
                        }
                    }

                    // alphanum unops
                    [Atom(S(op1)), _, ..] if UNOPS.contains(op1.as_str()) => {
                        // std::mem::take(op1); // now it shows as Lexpr::Stolen in list_contents
                        // let rest: Vec<_> = rest.iter_mut().map(|x| std::mem::take(x)).collect();
                        let uop_type = match op1.as_str() {
                            "add1" => Op1::Add1,
                            "sub1" => Op1::Sub1,
                            "print" => Op1::Print,
                            "!" => Op1::Not,
                            s => panic!("Invalid: Unknown unary operation {:?}", s),
                        };

                        list_contents.remove(0); // drop op1
                        self.push_state(ParserState::new(
                            vec![package_lexr_vec(list_contents)],
                            move |mut parsed| {
                                if parsed.len() != 1 {
                                    unreachable!(
                                        "we expect parsed to have the same length as unparded"
                                    )
                                };
                                let parsed = parsed.pop().unwrap();

                                Expr::UnOp(uop_type, Box::new(parsed))
                            },
                        ))
                    }

                    // assume any vector in the form <wrapped_expr>(paren_list) is a function call
                    [fn_name_or_ptr, ParenList(fun_args)]
                        if is_wrapped_expr_non_kword(fn_name_or_ptr) =>
                    {
                        let mut fun_args = std::mem::take(fun_args);
                        let fn_name_or_ptr = std::mem::take(fn_name_or_ptr);

                        let fun_name = match &fn_name_or_ptr {
                            Atom(S(fun_name)) => {
                                Some(Expr::FunName(name_to_string(&fun_name, NameType::FunName)))
                            }
                            _lexpr => None,
                        };

                        let unparsed = match fun_name {
                            Some(_) => fun_args,
                            None => {
                                fun_args.push(fn_name_or_ptr); // need to parse fun pointer
                                fun_args
                            }
                        };
                        if unparsed.is_empty() {
                            self.consume_expr(Expr::Call(Box::new(fun_name.unwrap()), vec![]))
                        } else {
                            self.push_state(ParserState::new(
                                unparsed,
                                |mut parsed| match fun_name {
                                    Some(name) => Expr::Call(Box::new(name), parsed),
                                    None => {
                                        let parsed_ptr = parsed.pop().unwrap();
                                        Expr::Call(Box::new(parsed_ptr), parsed)
                                    }
                                },
                            ))
                        }
                    }

                    // binops
                    [lhs, Atom(S(op2)), rhs] if BET_BINOPS.contains(&op2.as_str()) => {
                        let binop = match op2.as_str() {
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
                        self.push_state(ParserState::new(
                            vec![std::mem::take(lhs), std::mem::take(rhs)],
                            move |mut parsed| {
                                if parsed.len() == 2 {
                                    let parsed_rhs = parsed.pop().unwrap();
                                    let parsed_lhs = parsed.pop().unwrap();
                                    Expr::BinOp(binop, Box::new(parsed_lhs), Box::new(parsed_rhs))
                                } else {
                                    panic!("expected to get same number of parsed as unparsed")
                                }
                            },
                        ))
                    }

                    // do {} while () loop
                    [Atom(S(do_kwd)), body_block, Atom(S(while_kwd)), ParenList(cond)]
                        if do_kwd == "do" && while_kwd == "until" =>
                    {
                        if cond.len() != 1 {
                            panic!(
                                "Got a cond for a do-while loop with length != 1: {:?}",
                                cond
                            )
                        }

                        let cond = cond.pop().unwrap();
                        let body_block = expect_block(std::mem::take(body_block)).unwrap();
                        self.push_state(ParserState::new(vec![body_block, cond], |mut parsed| {
                            if parsed.len() == 2 {
                                let parsed_cond = parsed.pop().unwrap();
                                let parsed_body = parsed.pop().unwrap();
                                Expr::RepeatUntil(Box::new(parsed_body), Box::new(parsed_cond))
                            } else {
                                panic!("expected to get same number of parsed as unparsed")
                            }
                        }))
                    }

                    // set!
                    // since this case is below the "let" case, we can match on rest
                    [Atom(S(_id)), Atom(S(walrus)), _, ..] if walrus == ":=" => {
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

                        let id = id_to_string(&id);
                        self.push_state(ParserState::new(
                            vec![package_lexr_vec(list_contents)],
                            |mut parsed| {
                                if parsed.len() == 1 {
                                    let parsed_new_val = parsed.pop().unwrap();
                                    Expr::Set(id, Box::new(parsed_new_val))
                                } else {
                                    panic!("expected to get same number of parsed as unparsed")
                                }
                            },
                        ))
                    }

                    // null TODO SUPPORT ARR AND FUN PTRS
                    [Atom(S(null_kwd)), Atom(S(struct_name))] if null_kwd == "null" => self
                        .consume_expr(Expr::Null(name_to_string(
                            &struct_name,
                            NameType::StructName,
                        ))),

                    // new (alloc) TODO SUPPORT ARR AND FUN PTRS
                    [Atom(S(new_kwd)), Atom(S(struct_name))] if new_kwd == "new" => self
                        .consume_expr(Expr::Alloc(name_to_string(
                            &struct_name,
                            NameType::StructName,
                        ))),

                    // lookup: <some things>.field_name
                    [.., _, Atom(S(dot)), Atom(S(_field_name))] if dot == "." => {
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
                        
                        self.push_state(ParserState::new(
                            vec![package_lexr_vec(list_contents)],
                            move |mut parsed| {
                                if parsed.len() == 1 {
                                    let parsed_val = parsed.pop().unwrap();
                                    Expr::Lookup(
                                        Box::new(parsed_val),
                                        name_to_string(&field_name, NameType::StructFieldName),
                                    )
                                } else {
                                    panic!("expected to get same number of parsed as unparsed")
                                }
                            },
                        ))
                    }

                    // update: <some things>.field_name := ()
                    [.., _, Atom(S(dot)), Atom(S(_field_name)), Atom(S(walrus)), _new_val]
                        if dot == "." && walrus == ":=" =>
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

                        self.push_state(ParserState::new(
                            vec![package_lexr_vec(list_contents), new_val],
                            move |mut parsed| {
                                if parsed.len() == 2 {
                                    let parsed_new_val = parsed.pop().unwrap();
                                    let parsed_struct_val = parsed.pop().unwrap();
                                    Expr::Update(
                                        Box::new(parsed_struct_val),
                                        name_to_string(&field_name, NameType::StructFieldName),
                                        Box::new(parsed_new_val),
                                    )
                                } else {
                                    panic!("expected to get same number of parsed as unparsed")
                                }
                            },
                        ))
                    }
                    [] => self.consume_expr(Expr::Unit), // empty List is unit

                    // TODO ARR LOOKUP AND UPDATE WITH []
                    // - null check
                    // - length check
                    _ => panic!("Invalid list: {:#?}", list_contents),
                },

                // CurlyList: a block
                CurlyList(exprns) => {
                    if exprns.len() == 0 {
                        self.consume_expr(Expr::Unit)
                    } else {
                        let new_context = ParserState::new(exprns, Expr::Block);
                        self.push_state(new_context)
                    }
                }

                // ParenList: a sequence of lexprs which were wrapped in parens
                // at this point, we only expect there to be one element-- these parens only exist for lexing / are additional parens-- or none, which indicates unit
                ParenList(mut vec) => {
                    match vec.len().cmp(&1) {
                        std::cmp::Ordering::Less => self.consume_expr(Expr::Unit), // zero
                        std::cmp::Ordering::Equal => ParseRetval::KeepGoing(vec.pop().unwrap()),
                        std::cmp::Ordering::Greater => panic!("Invalid parens: {:?}.", vec),
                    }
                }
                Lexpr::Stolen => unreachable!(), // stolen should be filtered by ParseState::new()
            };
            match parse_subres {
                ParseRetval::Done(res) => return res,
                ParseRetval::KeepGoing(parse_next) => currently_parsing = parse_next,
            }
        }
    }
}

fn parse_expr(lexpr: Lexpr) -> Expr {
    IterativeParser::new().parse_lexpr(lexpr)
}

fn expect_block(lexpr: Lexpr) -> Result<Lexpr, String> {
    if let CurlyList(_stuff) = lexpr {
        Ok(CurlyList(_stuff))
    } else {
        Err(format!(
            "Tried to parse a block, but didn't find curly braces: {:?}",
            lexpr
        ))
    }
}

/// Parses a slice of Lexrp into an ExprType
///
/// Handles:
/// - base types: `unit`, `int`, `bool`
/// - structs
/// - function pointers
fn parse_type(type_annotation: &[Lexpr]) -> ExprType {
    match type_annotation {
        // struct, base type
        [Atom(S(single_type))] => single_type_str_to_expr_type(single_type),

        // function type
        [ParenList(arg_types), Atom(S(arrow)), ret_type @ ..] if arrow == "->" => {
            let parsed_arg_types = arg_types
                .into_iter()
                .map(std::slice::from_ref)
                .map(parse_type)
                .collect();
            let parsed_ret_type: ExprType = parse_type(ret_type);
            ExprType::FunctionPointer(parsed_arg_types, Box::new(parsed_ret_type))
        }

        _ => panic!("Invalid type annotation: {:?}", type_annotation),
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
        // single type
        [Atom(S(field_name)), Atom(S(type_anotation)), field_type @ ..]
            if type_anotation == "::" =>
        {
            // Will check field isn't a duplicate in type-check
            (
                parse_type(field_type),
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

    // create a unique type enumeration for the struct, if it doesn't yet exist, panic if keyword
    single_type_str_to_expr_type(struct_name);

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
        [Atom(S(fun_keyword)), Atom(S(fun_name)), ParenList(params), Atom(S(type_annotation_kword)), ret_type @ .., fun_body]
            if fun_keyword == "fun" && type_annotation_kword == "::" =>
        {
            (fun_name, params, ret_type, fun_body)
        }
        _ => return None,
    };

    // get return type
    let ret_type = parse_type(ret_type);

    // from here on, we know it must be a function declaration; so panics instead of None

    // check name (panics if keyword)
    let fun_name = name_to_string(fun_name, NameType::FunName);

    // parse params
    let params_vec = params
        .into_iter()
        .map(|param| parse_type_annotated_name(s, param))
        .collect();

    Some(UserFunction::UserFun(
        fun_name,
        FunSignature::Sig(ret_type, params_vec),
        parse_expr(expect_block(fun_body.clone()).unwrap()), // TODO: CLONE
    ))
}

pub fn parse_prog(lexpr: &mut Lexpr) -> Prog {
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
    Prog::Program(struct_defs, fn_defs, parse_expr(std::mem::take(body))) // TODO: CLONE
}

fn is_wrapped_expr_non_kword(lexpr: &Lexpr) -> bool {
    match lexpr {
        Atom(S(s)) => !is_keyword(s),
        CurlyList(_) => true,
        ParenList(_) => true,
        _ => false,
    }
}
