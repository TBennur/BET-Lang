use std::{collections::VecDeque, u32};

use crate::{
    alt_stack::*,
    compile::StructSerializer,
    semantics::{struct_name_to_type_enum, struct_type_enum_to_name},
    stack::StackState,
    structs::*,
    typecheck_fast_shared::{
        struct_sig_type_of, validate_type, validate_type_fast, ExpectedType, TypeCheckState,
        WrappedFastExpr,
    },
};

trait ConsumeConstruct<From, To> {
    type State<'b>;

    fn construct<'b>(&mut self, parsed: &mut Vec<To>, stack: &mut Self::State<'b>) -> To;

    fn consume<'b>(&mut self, consume: &To, stack: &mut Self::State<'b>) -> Option<From>;
}

enum BinOpConstructor<'a, 'b> {
    Equal { b_vec: Vec<WrappedFastExpr<'a, 'b>> },
    Other(Op2),
}

enum TypeChecker<'a, 'b> {
    __INTERNAL,
    UnOp(Op1, ExpectedType),
    Set {
        t1: FastExprType,
        name: &'a str,
        expected: ExpectedType,
    },
    BinOp(BinOpConstructor<'a, 'b>, ExpectedType),
    RepeatUntil {
        expected: ExpectedType,
    },
    Block {
        expected: ExpectedType,
        expected_len: usize,
    },
    Let {
        expected: ExpectedType,
        ids: Vec<&'a str>,

        // TODO: consider moving into Box of another struct, to shrink size for other variants
        next_id_ind: usize,
        curr_type_bindings: im::HashMap<&'a str, FastExprType>,
        binding_exprs: VecDeque<FastExpr<'a>>,
        finially: Option<FastExpr<'a>>,
        struct_hm: &'b im::HashMap<&'a str, i32>,
        function_sigs: &'b im::HashMap<&'a str, (FastFunSignature<'a>, ExprType)>,
        struct_sigs: &'b im::HashMap<&'a str, FastStructSignature<'a>>,
        allow_input: bool,
    },
    ArrLen {
        expected: ExpectedType,
    },
    ArrayAlloc {
        expected: ExpectedType,
        arr_t: FastExprType,
    },
    ArrayLookup {
        expected: ExpectedType,
    },
    Lookup {
        expected: ExpectedType,
        field_name: &'a str,
        struct_sigs: &'b im::HashMap<&'a str, FastStructSignature<'a>>,
    },
    If {
        untyped: Vec<WrappedFastExpr<'a, 'b>>,
        expected: ExpectedType,
    },
    Call {
        expected: ExpectedType,
        arg_len: usize,
        ptr_checked: bool,
        arguments: Vec<WrappedFastExpr<'a, 'b>>,
    },
    Update {
        expected: ExpectedType,
        field_name: &'a str,
        new_value: Option<WrappedFastExpr<'a, 'b>>,
    },
    ArrayUpdate {
        expected: ExpectedType,
        new_val: Option<WrappedFastExpr<'a, 'b>>,
        parsed_ptr: bool,
    },
}

struct ConsumeStackState<From, To, C>
where
    C: ConsumeConstruct<From, To>,
{
    unparsed: VecDeque<From>, // use VecDeque so we can pop from the front
    parsed: Vec<To>,
    constructor: C,
}

impl<'b, From, To, C: ConsumeConstruct<From, To>> StackStateAble<C::State<'b>>
    for ConsumeStackState<From, To, C>
{
    type From = From;

    type To = To;

    fn consume_plus(
        &mut self,
        expr: Self::To,
        state: &mut C::State<'b>,
    ) -> StackRetval<Self::From, Self::To> {
        if let Some(append_to_unparsed) = self.constructor.consume(&expr, state) {
            self.unparsed.push_front(append_to_unparsed); // TODO should this be push back or push front?
        }

        self.parsed.push(expr);

        match self.unparsed.pop_front() {
            None => StackRetval::Done(self.constructor.construct(&mut self.parsed, state)),
            Some(next) => StackRetval::KeepGoing(next),
        }
    }

    fn get_next(&mut self) -> StackRetval<Self::From, Self::To> {
        StackRetval::KeepGoing(self.unparsed.pop_front().unwrap())
    }
}

impl<From, To, C> ConsumeStackState<From, To, C>
where
    C: ConsumeConstruct<From, To>,
{
    fn new(unparsed: VecDeque<From>, constructor: C) -> Self {
        ConsumeStackState {
            unparsed,
            parsed: Vec::new(),
            constructor,
        }
    }
}

impl<'a, 'b> ConsumeConstruct<WrappedFastExpr<'a, 'b>, FastTypedExpr<'a>> for TypeChecker<'a, 'b> {
    type State<'c> = TypeCheckState;

    fn construct<'c>(
        &mut self,
        typed: &mut Vec<FastTypedExpr<'a>>,
        state: &mut Self::State<'b>,
    ) -> FastTypedExpr<'a> {
        match self {
            TypeChecker::__INTERNAL => unreachable!(),

            TypeChecker::UnOp(op1, expected) => {
                let op1 = *op1;
                if typed.len() != 1 {
                    panic!("bad len")
                }
                let typed_expr = typed.pop().unwrap();

                match op1 {
                    Op1::Add1 | Op1::Sub1 => {
                        FastTypedExpr::UnOp(FastExprType::Int, op1, Box::new(typed_expr))
                    }
                    Op1::Not => {
                        FastTypedExpr::UnOp(typed_expr.extract_type(), op1, Box::new(typed_expr))
                    }
                    Op1::Print => {
                        FastTypedExpr::UnOp(typed_expr.extract_type(), op1, Box::new(typed_expr))
                    }
                }
            }
            .check_type(*expected),

            TypeChecker::Set { t1, name, expected } => {
                let t1 = *t1;
                if typed.len() != 1 {
                    panic!("bad typed len")
                }
                let t1_prime = typed.pop().unwrap();

                FastTypedExpr::Set(t1, name, Box::new(t1_prime))
            }
            .check_type(*expected),

            TypeChecker::BinOp(bin_op_constructor, expected) => match bin_op_constructor {
                BinOpConstructor::Equal { b_vec: _ } => {
                    assert!(typed.len() == 2);
                    let b_typed: FastTypedExpr = typed.pop().unwrap();
                    let a_typed = typed.pop().unwrap();
                    FastTypedExpr::BinOp(
                        FastExprType::Bool,
                        Op2::Equal,
                        Box::new(a_typed),
                        Box::new(b_typed),
                    )
                }
                .check_type(*expected),

                BinOpConstructor::Other(op2) => {
                    let op2 = *op2;
                    let expr_type = match op2 {
                        Op2::Plus | Op2::Minus | Op2::Times => FastExprType::Int,
                        Op2::Or
                        | Op2::And
                        | Op2::Greater
                        | Op2::GreaterEqual
                        | Op2::Less
                        | Op2::LessEqual => FastExprType::Bool,
                        Op2::Equal => unreachable!(),
                    };

                    assert!(typed.len() == 2);
                    let b_typed = typed.pop().unwrap();
                    let a_typed = typed.pop().unwrap();

                    FastTypedExpr::BinOp(expr_type, op2, Box::new(a_typed), Box::new(b_typed))
                }
                .check_type(*expected),
            },

            TypeChecker::RepeatUntil { expected } => {
                assert_eq!(typed.len(), 2);
                let typed_body: FastTypedExpr = typed.pop().unwrap();
                let typed_stop_cond = typed.pop().unwrap();
                FastTypedExpr::RepeatUntil(
                    typed_body.extract_type(),
                    Box::new(typed_body),
                    Box::new(typed_stop_cond),
                )
            }
            .check_type(*expected),

            TypeChecker::Block {
                expected,
                expected_len,
            } => {
                let typed = std::mem::take(typed);
                let expected_len = *expected_len;
                assert_eq!(expected_len, typed.len());

                // type of a block is type of last, or unit if empty
                let block_type = if typed.len() == 0 {
                    FastExprType::Unit
                } else {
                    typed.last().unwrap().extract_type()
                };

                FastTypedExpr::Block(block_type, typed)
            }
            .check_type(*expected),
            TypeChecker::Let { ids, expected, .. } => {
                let num_bindings = ids.len();
                let mut typed = std::mem::take(typed);
                let ids = std::mem::take(ids);

                assert!(typed.len() == num_bindings + 1); // bindings plus body expression

                let body_texpr = typed.pop().unwrap(); // get body expression
                let typed_bindings: Vec<_> = ids.into_iter().zip(typed).collect();
                FastTypedExpr::Let(
                    body_texpr.extract_type(),
                    typed_bindings,
                    Box::new(body_texpr),
                )
            }
            .check_type(*expected),

            TypeChecker::ArrLen { expected } => {
                assert!(typed.len() == 1);
                let typed_arr = typed.pop().unwrap();
                FastTypedExpr::ArrayLen(FastExprType::Int, Box::new(typed_arr))
            }
            .check_type(*expected),

            TypeChecker::ArrayAlloc { expected, arr_t } => {
                assert!(typed.len() == 1);
                let typed = typed.pop().unwrap();
                FastTypedExpr::ArrayAlloc(*arr_t, Box::new(typed))
            }
            .check_type(*expected),

            TypeChecker::ArrayLookup { expected } => {
                assert!(typed.len() == 2);
                let typed_ind: FastTypedExpr = typed.pop().unwrap();
                let typed_arr = typed.pop().unwrap();

                let elem_type = match state.to_expr_type_ref(typed_arr.extract_type()) {
                    Some(ExprType::Array(elem_type)) => &**elem_type,
                    _ => panic!(
                        "expected array lookup to operate on array type, not on {:?}",
                        typed_arr
                    ),
                };

                let elem_t_fexpr = match state.to_fexpr_t_opt(elem_type) {
                    Some(t) => t,
                    None => state.ingest(elem_type.clone()).0,
                };

                FastTypedExpr::ArrayLookup(elem_t_fexpr, Box::new(typed_arr), Box::new(typed_ind))
            }
            .check_type(*expected),
            TypeChecker::Lookup {
                expected,
                field_name,
                struct_sigs,
            } => {
                {
                assert!(typed.len() == 1);
                let typed_pointer = typed.pop().unwrap();

                // first expression must be a pointer type
                let pointed_struct_enum =
                    match state.to_expr_type_ref(typed_pointer.extract_type()) {
                        Some(ExprType::StructPointer(struct_enum)) => *struct_enum,
                        _ => panic!(
                            "Invalid: Lookup on non-pointer-typed expression: {:?}",
                            "todo", // pointer
                        ), // should never happen, since type_check_expr only returns well-typed pointers
                    };

                // the kind of struct it points to must have a corresponding field
                let pointed_struct_name = match struct_type_enum_to_name(pointed_struct_enum) {
                    Some(s) => s,
                    None => panic!(
                        "Invalid: Lookup with non-existent struct with type enumeration: {}",
                        pointed_struct_enum
                    ),
                };

                let struct_sig = match struct_sigs.get(pointed_struct_name.as_str()) {
                    Some(sig) => sig,
                    None => panic!(
                        "Unexpected: Lookup with non-existent struct: {}",
                        pointed_struct_name
                    ),
                };

                let expected_type = match struct_sig_type_of(struct_sig, &(*field_name).to_string()) { // TODO CLONE
                Some(expr_type) => expr_type,
                None => panic!("Invalid: Lookup nonexistent field {field_name} in struct {pointed_struct_name}"),
                };

                let fast_expected_type = state.ingest(expected_type).0;

                FastTypedExpr::Lookup(
                    fast_expected_type,
                    Box::new(typed_pointer),
                    field_name,
                )
            }.check_type(*expected)
            }
            TypeChecker::If { expected, .. } => {
                assert!(typed.len() == 3); // cond t_branch f_branch
                let f_branch: FastTypedExpr = typed.pop().unwrap();
                let t_branch = typed.pop().unwrap();
                let cond = typed.pop().unwrap();
                FastTypedExpr::If(
                    t_branch.extract_type(),
                    Box::new(cond),
                    Box::new(t_branch),
                    Box::new(f_branch),
                )
            }
            .check_type(*expected),

            TypeChecker::Call {
                expected, arg_len, ..
            } => {
                assert!(typed.len() == *arg_len + 1); // all args + fn ptr
                let typed_fun_ptr: FastTypedExpr = typed.remove(0); // TODO POP FRONT

                // can clean this up; and we never expect to need to clone here, as when creating the function type we should've ingested the ret type
                let fast_ret_type = match state.to_expr_type_ref(typed_fun_ptr.extract_type()) {
                    Some(ExprType::FunctionPointer(_, ret_type)) => {
                        match state.to_fexpr_t_opt(&ret_type) {
                            Some(t) => t,
                            None => unreachable!(),
                        }
                    }
                    _ => unreachable!("should have panicked earlier"),
                };
                // let fast_ret_type = state.ingest(ret_type).0; // TODO CLONE

                FastTypedExpr::Call(
                    fast_ret_type,
                    Box::new(typed_fun_ptr),
                    std::mem::take(typed),
                )
            }
            .check_type(*expected),

            TypeChecker::Update {
                field_name,
                expected,
                ..
            } => {
                assert!(
                    typed.len() == 2,
                    "expected length 2 but got {}",
                    typed.len()
                );
                let new_val_typed: FastTypedExpr = typed.pop().unwrap();
                let struct_ptr_typed = typed.pop().unwrap();
                FastTypedExpr::Update(
                    new_val_typed.extract_type(),
                    Box::new(struct_ptr_typed),
                    field_name,
                    Box::new(new_val_typed),
                )
            }
            .check_type(*expected),
            TypeChecker::ArrayUpdate { expected, .. } => {
                assert!(typed.len() == 3);
                // when we produce a the new_val in consume(), it is parsed before ind, so ind is parsed last
                let typed_ind = typed.pop().unwrap();
                let typed_new_val: FastTypedExpr = typed.pop().unwrap();
                let typed_arr_ptr = typed.pop().unwrap();

                let elem_type = match state.to_expr_type_ref(typed_arr_ptr.extract_type()) {
                    Some(ExprType::Array(elem_type)) => &**elem_type,
                    _ => panic!(
                        "expected array lookup to operate on array type, not on {:?}",
                        typed_arr_ptr
                    ),
                };

                let elem_t_fexpr = match state.to_fexpr_t_opt(elem_type) {
                    Some(t) => t,
                    None => state.ingest(elem_type.clone()).0,
                };

                FastTypedExpr::ArrayUpdate(
                    elem_t_fexpr,
                    Box::new(typed_arr_ptr),
                    Box::new(typed_ind),
                    Box::new(typed_new_val),
                )
            }
            .check_type(*expected),
        }
    }

    fn consume<'c>(
        &mut self,
        typed: &FastTypedExpr<'a>,
        state: &mut Self::State<'b>,
    ) -> Option<WrappedFastExpr<'a, 'b>> {
        match self {
            // produce when consuming
            TypeChecker::BinOp(BinOpConstructor::Equal { b_vec }, _expected_type) => {
                match b_vec.pop() {
                    Some(mut wrapped) => {
                        assert!(
                            wrapped.expected_type
                                == ExpectedType::Specific(FastExprType::Other(u32::MAX))
                        );

                        wrapped.expected_type = ExpectedType::Specific(typed.extract_type());
                        Some(wrapped)
                    }
                    None => None,
                }
            }

            TypeChecker::Let {
                ids,
                next_id_ind,
                curr_type_bindings: type_bindings,
                binding_exprs,
                struct_hm,
                function_sigs,
                struct_sigs,
                allow_input,
                finially,
                ..
            } => {
                let num_bindings = ids.len();
                let allow_input = *allow_input;
                let curr_ind = *next_id_ind;
                assert!(curr_ind <= num_bindings);

                // if, based on next_id_ind, typed is a binding, update type_binding
                if let Some(&id) = ids.get(curr_ind) {
                    let fast_expr_type = typed.extract_type();
                    type_bindings.insert(id, fast_expr_type);
                    *next_id_ind += 1; // move to next binding
                }

                // try to parse the next binding
                if let Some(next_binding) = binding_exprs.pop_front() {
                    // parse the next binding with updated type map
                    return Some(WrappedFastExpr {
                        fast_expr: next_binding,
                        struct_hm,
                        expected_type: ExpectedType::Any, // variable can have any type
                        type_bindings: type_bindings.clone(),
                        function_sigs,
                        struct_sigs,
                        allow_input,
                    });
                }

                // try to parse the let body once we've typechecked all bindings
                match std::mem::take(finially) {
                    Some(body) => {
                        Some(WrappedFastExpr {
                            fast_expr: body,
                            struct_hm,
                            expected_type: ExpectedType::Any, // let can have any type
                            type_bindings: type_bindings.clone(),
                            function_sigs,
                            struct_sigs,
                            allow_input,
                        })
                    }
                    None => None,
                }
            }

            TypeChecker::If { untyped, .. } => match untyped.pop() {
                None => None, // done (will construct)
                Some(mut untyped_expr) => {
                    let expected_type = if untyped.len() == 1 {
                        // first branch can be any type
                        ExpectedType::Any
                    } else {
                        assert!(untyped.len() == 0);
                        // second branch must match first
                        ExpectedType::Specific(typed.extract_type())
                    };
                    untyped_expr.expected_type = expected_type;
                    Some(untyped_expr)
                }
            },

            TypeChecker::Call {
                arg_len,
                ptr_checked,
                arguments,
                ..
            } => {
                // the first typed is the function pointer, which dictates the type and number of arguments
                if !*ptr_checked {
                    let arg_types = match state.to_expr_type_ref(typed.extract_type()) {
                        Some(ExprType::FunctionPointer(arg_types, _ret_type)) => arg_types.clone(),
                        _ => panic!("not function pointe"),
                    };

                    if arg_types.len() != *arg_len {
                        panic!("wrong number of args")
                    }

                    for (expected, arg) in arg_types
                        .into_iter()
                        .map(|t| state.ingest(t).0)
                        .zip(arguments.iter_mut())
                    {
                        assert!(
                            arg.expected_type
                                == ExpectedType::Specific(FastExprType::Other(u32::MAX))
                        );

                        arg.expected_type = ExpectedType::Specific(expected);
                    }

                    arguments.reverse(); // so that popping takes in order (TODO: change)
                    *ptr_checked = true;
                }

                arguments.pop() // next argument, or None
            }

            // don't produce
            TypeChecker::UnOp(..) => None,
            TypeChecker::Set { .. } => None,
            TypeChecker::BinOp(BinOpConstructor::Other(_op2), _expected) => None,
            TypeChecker::RepeatUntil { .. } => None,
            TypeChecker::Block { .. } => None,
            TypeChecker::ArrLen { .. } => None,
            TypeChecker::ArrayAlloc { .. } => None,
            TypeChecker::ArrayLookup { .. } => None,
            TypeChecker::Lookup { .. } => None,

            // invalid states
            TypeChecker::__INTERNAL => unreachable!(),

            TypeChecker::Update {
                field_name,
                new_value,
                ..
            } => match std::mem::take(new_value) {
                None => None,
                Some(mut new_value) => {
                    // check the pointer type
                    let pointed_struct_enum = match state.to_expr_type_ref(typed.extract_type()) {
                        Some(ExprType::StructPointer(struct_enum)) => *struct_enum,
                        _ => panic!(
                            "Invalid: Update on non-pointer-typed expression: {:?}",
                            "todo" // pointer
                        ),
                    };

                    // the kind of struct it points to must have a corresponding field
                    let pointed_struct_name = match struct_type_enum_to_name(pointed_struct_enum) {
                        Some(s) => s,
                        None => panic!(
                            // type enum doesn't exist; should never happen
                            "Unexpected: Update with non-existent struct with type enumeration: {}",
                            pointed_struct_enum
                        ),
                    };

                    let struct_sig = match new_value.struct_sigs.get(pointed_struct_name.as_str()) {
                        Some(sig) => sig, // struct name not in struct_sigs; shouldn't happen
                        None => panic!(
                            "Unexpected: Update with non-existent struct signature: {}",
                            pointed_struct_name
                        ),
                    };

                    // this is an error which could actually happen
                    let expected_type = match struct_sig_type_of(struct_sig, &field_name) {
                                Some(expr_type) => expr_type,
                                None => panic!(
                                "Invalid: tried to update field name {} of struct {} which has no such field",
                                pointed_struct_name, field_name
                            ),
                            };

                    new_value.expected_type = ExpectedType::Specific(state.ingest(expected_type).0);

                    // parse the new value
                    Some(new_value)
                }
            },

            TypeChecker::ArrayUpdate {
                expected: _,
                new_val,
                parsed_ptr,
            } => {
                if *parsed_ptr {
                    return None;
                }

                let typed_arr = typed;
                *parsed_ptr = true;

                let elem_type = match state.to_expr_type_ref(typed_arr.extract_type()) {
                    Some(ExprType::Array(elem_type)) => &**elem_type,
                    _ => panic!(
                        "expected array lookup to operate on array type, not on {:?}",
                        typed_arr
                    ),
                };

                let elem_t_fexpr = match state.to_fexpr_t_opt(elem_type) {
                    Some(t) => t,
                    None => state.ingest(elem_type.clone()).0,
                };

                let mut new_val = std::mem::take(new_val).unwrap();
                new_val.expected_type = ExpectedType::Specific(elem_t_fexpr);
                Some(new_val)
            }
        }
    }
}

type TypecheckStack<'a, 'b> =
    ConsumeStackState<WrappedFastExpr<'a, 'b>, FastTypedExpr<'a>, TypeChecker<'a, 'b>>;

impl<'a, 'b> OneStep<TypeCheckState, TypecheckStack<'a, 'b>> for WrappedFastExpr<'a, 'b> {
    fn step(
        mut self,
        state: &mut TypeCheckState,
    ) -> StepResult<
        <TypecheckStack<'a, 'b> as StackStateAble<TypeCheckState>>::To,
        TypecheckStack<'a, 'b>,
    > {
        // extract the current expression we are taking
        let curr = self.fast_expr;
        self.fast_expr = FastExpr::__INTERNAL;

        let expected = self.expected_type;

        match curr {
            /* --- Always Terminal --- */
            FastExpr::Unit => StepResult::Terminal(FastTypedExpr::Unit.check_type(expected)),

            FastExpr::Input => {
                if !self.allow_input {
                    panic!("Invalid: Input is not an Int")
                }
                StepResult::Terminal(FastTypedExpr::Input.check_type(expected))
            }

            FastExpr::Boolean(b) => {
                StepResult::Terminal(FastTypedExpr::Boolean(b).check_type(expected))
            }

            FastExpr::Id(id) => match self.function_sigs.get(&id) {
                // first, check for function names (which resolve to function pointers)
                Some((_sig, ptr_type)) => StepResult::Terminal(
                    FastTypedExpr::FunName(state.ingest_clone(ptr_type), id).check_type(expected),
                ),

                // next, check for variables
                None => match self.type_bindings.get(id) {
                    None => panic!("Invalid: Unbound variable identifier {}", id),
                    Some(t) => StepResult::Terminal(FastTypedExpr::Id(*t, id).check_type(expected)),
                },
            },

            FastExpr::Number(n) => {
                StepResult::Terminal(FastTypedExpr::Number(n).check_type(expected))
            }

            FastExpr::FunName(fun_name) => match self.function_sigs.get(fun_name) {
                // check global functions first
                Some((_sig, ptr_type)) => {
                    let fast_ptr_type = state.ingest_clone(ptr_type);

                    StepResult::Terminal(
                        FastTypedExpr::FunName(fast_ptr_type, fun_name).check_type(expected),
                    )
                }

                None => {
                    // variables next
                    let fast_type = match self.type_bindings.get(&fun_name) {
                        None => panic!(
                            "Invalid: Called function {:?}, which doesn't exist",
                            fun_name
                        ),
                        Some(fast_type) => fast_type,
                    };

                    let type_expr = fast_type.to_expr_type_ref(state);
                    match type_expr {
                        ExprType::FunctionPointer(_arg_types, _ret_type) => StepResult::Terminal(
                            FastTypedExpr::Id(*fast_type, fun_name).check_type(expected),
                        ),

                        _ => panic!(
                            "Invalid: Called function {:?}, which doesn't exist",
                            fun_name
                        ),
                    }
                }
            },

            FastExpr::Null(struct_name) => {
                // check that struct type actually exists (and is well typed)
                if !self.struct_sigs.contains_key(struct_name) {
                    panic!(
                        "Invalid: null pointer to unknown struct type: {}",
                        struct_name
                    )
                } // struct_name is valid
                let struct_enum = struct_name_to_type_enum(&struct_name.to_string()); // TODO CLONE
                StepResult::Terminal(
                    FastTypedExpr::Null(state.ingest(ExprType::StructPointer(struct_enum)).0)
                        .check_type(expected),
                )
            }

            FastExpr::Alloc(struct_name) => {
                // check that struct type actually exists (and is well typed)
                if !self.struct_sigs.contains_key(struct_name) {
                    panic!("Invalid: alloc of unknown struct type: {}", struct_name)
                } // struct_name is valid
                let struct_enum = struct_name_to_type_enum(&struct_name.to_string());
                StepResult::Terminal(
                    FastTypedExpr::Alloc(state.ingest(ExprType::StructPointer(struct_enum)).0)
                        .check_type(expected),
                )
            }

            FastExpr::UnOp(op1, expr) => {
                let expected_type = match op1 {
                    Op1::Add1 | Op1::Sub1 => ExpectedType::Specific(state.ingest(ExprType::Int).0),
                    Op1::Not => ExpectedType::Specific(state.ingest(ExprType::Bool).0),
                    Op1::Print => ExpectedType::Any,
                };

                let wrapped: WrappedFastExpr = {
                    WrappedFastExpr {
                        fast_expr: *expr,
                        struct_hm: self.struct_hm,
                        expected_type,
                        type_bindings: self.type_bindings,
                        function_sigs: self.function_sigs,
                        struct_sigs: self.struct_sigs,
                        allow_input: self.allow_input,
                    }
                };

                StepResult::Nonterminal(ConsumeStackState::new(
                    VecDeque::from([wrapped]),
                    TypeChecker::UnOp(op1, expected),
                ))
            }

            FastExpr::Set(name, new_value) => {
                // fails if the name isn't in scope
                let t1 = match self.type_bindings.get(name) {
                    Some(&t1) => t1,
                    None => panic!("Invalid: Unbound variable identifier {}", name),
                };
                // can only "set" a variable to the same type within the current scope

                let expected_type = ExpectedType::Specific(t1);

                let wrapped_new_value: WrappedFastExpr<'a, 'b> = WrappedFastExpr {
                    fast_expr: *new_value,
                    struct_hm: self.struct_hm,
                    expected_type,
                    type_bindings: self.type_bindings,
                    function_sigs: self.function_sigs,
                    struct_sigs: self.struct_sigs,
                    allow_input: self.allow_input,
                };

                StepResult::Nonterminal(ConsumeStackState::new(
                    VecDeque::from([wrapped_new_value]),
                    TypeChecker::Set { t1, name, expected },
                ))
            }

            FastExpr::Let(bindings, finially) => {
                let num_bindings = bindings.len();
                assert!(num_bindings > 0);

                // Duplicate Id Check
                let mut in_this_let: im::HashSet<&'a str> = im::HashSet::new();
                for (id, _) in bindings.iter() {
                    match in_this_let.insert(*id) {
                        None => (),
                        Some(_) => panic!("Duplicate binding"),
                    };
                }

                let (ids, mut binding_exprs): (Vec<_>, VecDeque<_>) = bindings.into_iter().unzip();

                let parse_first = binding_exprs.pop_front().unwrap();
                let tc_first_wrapped: WrappedFastExpr<'a, 'b> = WrappedFastExpr {
                    fast_expr: parse_first,
                    struct_hm: self.struct_hm,
                    expected_type: ExpectedType::Any, // variables can have any type
                    type_bindings: self.type_bindings.clone(),
                    function_sigs: self.function_sigs,
                    struct_sigs: self.struct_sigs,
                    allow_input: self.allow_input,
                };

                StepResult::Nonterminal(ConsumeStackState::new(
                    VecDeque::from([tc_first_wrapped]),
                    TypeChecker::Let {
                        ids,
                        expected,
                        next_id_ind: 0,
                        curr_type_bindings: self.type_bindings,
                        binding_exprs,
                        finially: Some(*finially),
                        struct_hm: self.struct_hm,
                        function_sigs: self.function_sigs,
                        struct_sigs: self.struct_sigs,
                        allow_input: self.allow_input,
                    },
                ))
            }

            FastExpr::BinOp(op2, a, b) => match op2 {
                Op2::Equal => {
                    // parse the a, then when consuming a_typed, push b with expected type
                    let a_wrapped = WrappedFastExpr {
                        fast_expr: *a,
                        struct_hm: self.struct_hm,
                        expected_type: ExpectedType::Any,
                        type_bindings: self.type_bindings.clone(),
                        function_sigs: self.function_sigs,
                        struct_sigs: self.struct_sigs,
                        allow_input: self.allow_input,
                    };

                    let b_vec = vec![WrappedFastExpr {
                        fast_expr: *b,
                        struct_hm: self.struct_hm,
                        expected_type: ExpectedType::Specific(FastExprType::Other(u32::MAX)),
                        type_bindings: self.type_bindings.clone(),
                        function_sigs: self.function_sigs,
                        struct_sigs: self.struct_sigs,
                        allow_input: self.allow_input,
                    }];

                    StepResult::Nonterminal(ConsumeStackState::new(
                        VecDeque::from([a_wrapped]),
                        TypeChecker::BinOp(BinOpConstructor::Equal { b_vec }, expected),
                    ))
                }

                op2 => {
                    let (a_exp_type, b_exp_type) = match op2 {
                        Op2::Plus | Op2::Minus | Op2::Times => {
                            (FastExprType::Int, FastExprType::Int)
                        }
                        Op2::Or | Op2::And => (FastExprType::Bool, FastExprType::Bool),
                        Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                            (FastExprType::Int, FastExprType::Int)
                        }
                        Op2::Equal => unreachable!(),
                    };
                    let (a_fst_typ, b_fst_typ) = (a_exp_type, b_exp_type);

                    StepResult::Nonterminal(ConsumeStackState::new(
                        VecDeque::from([
                            WrappedFastExpr {
                                fast_expr: *a,
                                struct_hm: self.struct_hm,
                                expected_type: ExpectedType::Specific(a_fst_typ),
                                type_bindings: self.type_bindings.clone(),
                                function_sigs: self.function_sigs,
                                struct_sigs: self.struct_sigs,
                                allow_input: self.allow_input,
                            },
                            WrappedFastExpr {
                                fast_expr: *b,
                                struct_hm: self.struct_hm,
                                expected_type: ExpectedType::Specific(b_fst_typ),
                                type_bindings: self.type_bindings,
                                function_sigs: self.function_sigs,
                                struct_sigs: self.struct_sigs,
                                allow_input: self.allow_input,
                            },
                        ]),
                        TypeChecker::BinOp(BinOpConstructor::Other(op2), expected),
                    ))
                }
            },

            FastExpr::If(cond, val_if_true, val_if_false) => {
                let mut untyped = vec![
                    self.split_child(
                        *val_if_true,
                        ExpectedType::Specific(FastExprType::Other(u32::MAX)),
                    ),
                    self.split_child(
                        *val_if_false,
                        ExpectedType::Specific(FastExprType::Other(u32::MAX)),
                    ),
                ];
                untyped.reverse(); // so that first pop gives us val_if_true, second gives ups val_if_false

                StepResult::Nonterminal(ConsumeStackState::new(
                    VecDeque::from([WrappedFastExpr {
                        fast_expr: *cond,
                        struct_hm: self.struct_hm,
                        // cond must typecheck to bool
                        expected_type: ExpectedType::Specific(FastExprType::Bool),
                        type_bindings: self.type_bindings.clone(),
                        function_sigs: self.function_sigs,
                        struct_sigs: self.struct_sigs,
                        allow_input: self.allow_input,
                    }]),
                    TypeChecker::If { expected, untyped },
                ))
            }
            FastExpr::Call(fn_name_or_ptr, arguments) => {
                let wrapped_name_or_ptr = WrappedFastExpr {
                    fast_expr: *fn_name_or_ptr,
                    struct_hm: self.struct_hm,
                    expected_type: ExpectedType::Any,
                    type_bindings: self.type_bindings.clone(),
                    function_sigs: self.function_sigs,
                    struct_sigs: self.struct_sigs,
                    allow_input: self.allow_input,
                };

                let arg_len = arguments.len();
                let arguments = arguments
                    .into_iter()
                    .map(|fast_expr| {
                        self.split_child(
                            fast_expr,
                            ExpectedType::Specific(FastExprType::Other(u32::MAX)),
                        )
                    })
                    .collect();

                StepResult::Nonterminal(ConsumeStackState::new(
                    VecDeque::from([wrapped_name_or_ptr]),
                    TypeChecker::Call {
                        ptr_checked: false,
                        expected,
                        arg_len,
                        arguments,
                    },
                ))
            }

            FastExpr::Update(pointer, field_name, new_value) => {
                StepResult::Nonterminal(ConsumeStackState::new(
                    VecDeque::from([
                        self.split_child(*pointer, ExpectedType::Any), // must check that it has pointer type
                    ]),
                    TypeChecker::Update {
                        expected,
                        field_name,
                        new_value: Some(self.split_child(
                            *new_value,
                            ExpectedType::Specific(FastExprType::Other(u32::MAX)),
                        )),
                    },
                ))
            }

            FastExpr::ArrayUpdate(arr, ind, new_val) => {
                StepResult::Nonterminal(ConsumeStackState::new(
                    VecDeque::from([
                        self.split_child(*arr, ExpectedType::Any),
                        self.split_child(*ind, ExpectedType::Specific(FastExprType::Int)),
                    ]),
                    TypeChecker::ArrayUpdate {
                        expected,
                        new_val: Some(self.split_child(
                            *new_val,
                            ExpectedType::Specific(FastExprType::Other(u32::MAX)),
                        )),
                        parsed_ptr: false,
                    },
                ))
            }

            FastExpr::RepeatUntil(body, stop_cond) => {
                StepResult::Nonterminal(ConsumeStackState::new(
                    VecDeque::from([
                        WrappedFastExpr {
                            fast_expr: *stop_cond,
                            struct_hm: self.struct_hm,
                            expected_type: ExpectedType::Specific(FastExprType::Bool),
                            type_bindings: self.type_bindings.clone(),
                            function_sigs: self.function_sigs,
                            struct_sigs: self.struct_sigs,
                            allow_input: self.allow_input,
                        },
                        WrappedFastExpr {
                            fast_expr: *body,
                            struct_hm: self.struct_hm,
                            expected_type: ExpectedType::Any,
                            type_bindings: self.type_bindings.clone(),
                            function_sigs: self.function_sigs,
                            struct_sigs: self.struct_sigs,
                            allow_input: self.allow_input,
                        },
                    ]),
                    TypeChecker::RepeatUntil { expected },
                ))
            }

            FastExpr::Block(expns) => {
                if expns.is_empty() {
                    panic!("Invalid: Block is Empty") // TODO CHANGE TO UNIT
                }
                let wrapped: VecDeque<_> = expns
                    .into_iter()
                    .map(|typed| WrappedFastExpr {
                        fast_expr: typed,
                        struct_hm: self.struct_hm,
                        expected_type: ExpectedType::Any,
                        type_bindings: self.type_bindings.clone(),
                        function_sigs: self.function_sigs,
                        struct_sigs: self.struct_sigs,
                        allow_input: self.allow_input,
                    })
                    .collect();

                let expected_len = wrapped.len();

                StepResult::Nonterminal(ConsumeStackState::new(
                    wrapped,
                    TypeChecker::Block {
                        expected_len,
                        expected,
                    },
                ))
            }

            FastExpr::Lookup(pointer, field_name) => {
                StepResult::Nonterminal(ConsumeStackState::new(
                    VecDeque::from([
                        self.split_child(*pointer, ExpectedType::Any), // must check
                    ]),
                    TypeChecker::Lookup {
                        expected,
                        field_name,
                        struct_sigs: self.struct_sigs,
                    },
                ))
            }

            FastExpr::ArrayLookup(arr, ind) => StepResult::Nonterminal(ConsumeStackState::new(
                VecDeque::from([
                    self.split_child(*arr, ExpectedType::Any),
                    self.split_child(*ind, ExpectedType::Specific(FastExprType::Int)),
                ]),
                TypeChecker::ArrayLookup { expected },
            )),

            FastExpr::ArrayAlloc(elem_t, len) => {
                let arr_t = ExprType::Array(Box::new(elem_t.clone()));
                let arr_t = state.ingest(arr_t).0;

                let elem_t = state.ingest(elem_t).0;
                validate_type_fast(&elem_t, self.struct_hm, state).unwrap();

                StepResult::Nonterminal(ConsumeStackState::new(
                    VecDeque::from([WrappedFastExpr {
                        fast_expr: *len,
                        struct_hm: self.struct_hm,
                        expected_type: ExpectedType::Specific(FastExprType::Int),
                        type_bindings: self.type_bindings,
                        function_sigs: self.function_sigs,
                        struct_sigs: self.struct_sigs,
                        allow_input: self.allow_input,
                    }]),
                    TypeChecker::ArrayAlloc { expected, arr_t },
                ))
            }

            FastExpr::ArrayLen(arr) => StepResult::Nonterminal(ConsumeStackState::new(
                VecDeque::from([self.split_child_inherit(*arr, ExpectedType::Any)]),
                TypeChecker::ArrLen { expected },
            )),

            FastExpr::__INTERNAL => unreachable!(),
        }
    }
}

/* Prog / Function / Struct Typechecking */

impl<'a> FastUserFunction<'a> {
    fn typecheck_alt(
        self,
        type_set: &mut TypeCheckState,
        struct_sigs: im::HashMap<&'a str, FastStructSignature<'a>>,
        function_sigs: im::HashMap<&'a str, (FastFunSignature<'a>, ExprType)>,
        struct_hm: im::HashMap<&'a str, i32>,
    ) -> FastTypedFunction<'a> {
        let FastUserFunction::UserFun(name, FastFunSignature::Sig(ret_type, param_types), body) =
            self;

        // insert args into type bindings
        let mut type_bindings: im::HashMap<&'a str, FastExprType> = im::HashMap::new();
        for (param_type, param_name) in param_types.iter() {
            match type_bindings.get(param_name) {
                Some(_) => panic!("Duplicate Argument"),
                None => type_bindings.insert(param_name, type_set.ingest_clone(param_type)), // TODO CLONE
            };
        }

        // get actual return type of body
        let wrapped_body = WrappedFastExpr {
            fast_expr: body,
            expected_type: ExpectedType::Specific(type_set.ingest_clone(&ret_type)),
            type_bindings,
            struct_hm: &struct_hm,
            function_sigs: &function_sigs,
            struct_sigs: &struct_sigs,
            allow_input: false,
        };

        let mut stack: IterativeStack<ConsumeStackState<_, _, _>, TypeCheckState> =
            IterativeStack::new(type_set);
        let type_checked_body = stack.iterate(wrapped_body);

        FastTypedFunction::Fun(
            name,
            FastFunSignature::Sig(ret_type, param_types),
            type_checked_body,
        )
    }
}

impl<'a> FastProg<'a> {
    pub fn typecheck_alt(self) -> FastTypedProg<'a> {
        let FastProg::Program(structs, functions, body) = self;

        /* --- Typecheck Structs --- */

        // read all structs into a map of struct name => type enum
        let mut struct_enum_map: im::HashMap<&'a str, i32> = im::HashMap::new();
        for FastUserStruct::UserStruct(struct_name, _) in structs.iter() {
            // get a type enumeration for our struct name, creating it if it doens't exist
            let our_type_enum = struct_name_to_type_enum(&struct_name.to_string()); // TODO STRING

            // check if the struct was already declared
            match struct_enum_map.get(struct_name) {
                Some(_) => panic!("Invalid: duplicate struct declaration: {}", struct_name),
                None => {
                    // map struct name => type enum
                    // unlike the more general maps in semantics.rs, this maps only declared structs
                    struct_enum_map.insert(struct_name, our_type_enum);
                    // TODO CLONE
                }
            }
        } // struct_enum_map maps declared struct names => type enumeration

        // typecheck each struct (ie, all fields valid), building a map of {struct type enumeration => StructType }
        let mut struct_type_map: im::HashMap<&'a str, FastStructSignature> = im::HashMap::new();
        let mut struct_layouts: im::HashMap<&'a str, FastStructLayout> = im::HashMap::new();
        for FastUserStruct::UserStruct(struct_name, FastStructSignature::Sig(field_names)) in
            structs
        {
            let mut checked_struct_fields: Vec<(ExprType, &'a str)> = Vec::new();
            let mut struct_fields_names: im::HashMap<&'a str, i32> = im::HashMap::new();
            let mut i = 0;
            for (field_type, field_name) in field_names {
                if let Some(_) = struct_fields_names.insert(field_name, i) {
                    panic!(
                        "Invalid: Duplicate field {:?} in struct {:?}",
                        field_name, struct_name
                    );
                }
                i += 1;

                match validate_type(&field_type, &struct_enum_map).err() {
                    None => (),
                    Some(err_str) => panic!(
                        "Invalid: struct {struct_name} had error {err_str} in field {field_name}"
                    ),
                }

                // field type has been checked
                let checked_field_type = field_type;
                checked_struct_fields.push((checked_field_type, field_name));
            }

            let struct_sig = FastStructSignature::Sig(checked_struct_fields);
            let struct_layout = FastStructLayout::Layout(struct_fields_names);

            // push this sig into the struct type map
            struct_type_map.insert(struct_name, struct_sig);
            struct_layouts.insert(struct_name, struct_layout);
        }

        /* --- Typecheck Functions --- */

        // read all functions into map of function name to type, checking for dupes and illegal names
        let mut function_sigs: im::HashMap<&'a str, (FastFunSignature, ExprType)> =
            im::HashMap::new();
        for FastUserFunction::UserFun(name, function_sig, _) in functions.iter() {
            match function_sigs.get(name) {
                Some(_) => panic!("Duplicate Function Definition"),
                None => {
                    let FastFunSignature::Sig(expr_type, vec) = &function_sig;
                    validate_type(expr_type, &struct_enum_map).unwrap();
                    for (v, _) in vec {
                        validate_type(v, &struct_enum_map).unwrap();
                    }

                    let ptr_type = function_sig.get_ptr_type();
                    function_sigs.insert(name, (function_sig.clone(), ptr_type));
                }
            };
        }

        let mut type_map: TypeCheckState = TypeCheckState::new();

        let typed_functions = functions
            .into_iter()
            .map(|f| {
                f.typecheck_alt(
                    &mut type_map,
                    struct_type_map.clone(),
                    function_sigs.clone(),
                    struct_enum_map.clone(),
                )
            })
            .collect();

        /* --- Check that no functions have the same name as any struct --- */

        for (struct_name, _) in struct_type_map.clone() {
            if let Some(_) = function_sigs.get(&struct_name) {
                panic!(
                    "Invalid: struct and fun declarations with the same name: {}",
                    struct_name
                );
            }
        }

        /* --- Typecheck Program Body --- */

        // allow input in the body of the program
        let wrapped_body = WrappedFastExpr {
            fast_expr: body,
            expected_type: ExpectedType::Any,
            type_bindings: im::HashMap::new(),
            struct_hm: &struct_enum_map,
            function_sigs: &function_sigs,
            struct_sigs: &struct_type_map,
            allow_input: true,
        };

        let mut iterative_stack: IterativeStack<ConsumeStackState<_, _, _>, TypeCheckState> =
            IterativeStack::new(&mut type_map);
        let typed_body: FastTypedExpr<'a> = iterative_stack.iterate(wrapped_body);

        FastTypedProg::Program(
            typed_body.extract_type(),
            type_map,
            struct_type_map,
            struct_layouts,
            typed_functions,
            typed_body,
        )
    }
}
