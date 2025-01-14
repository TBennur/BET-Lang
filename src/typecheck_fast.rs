use crate::semantics::struct_name_to_type_enum;
use crate::stack::*;

use crate::typecheck_fast_shared::{
    struct_sig_type_of, validate_type, validate_type_fast, ExpectedType, TypeCheckState,
    WrappedFastExpr,
};
use crate::{semantics::struct_type_enum_to_name, stack::StepResult, structs::*};

/* --- Stack based typechecking --- */

impl<'a, 'b> OneStep<'b, FastTypedExpr<'a>, TypeCheckState> for WrappedFastExpr<'a, 'b> {
    fn step(
        mut self,
        state: &mut TypeCheckState,
    ) -> StepResult<'b, Self, FastTypedExpr<'a>, TypeCheckState> {
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

            /* --- Non-Terminal --- */
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

                StepResult::Nonterminal(StackState::new(vec![wrapped], move |mut typed, _| {
                    {
                        if typed.len() != 1 {
                            panic!("bad len")
                        }
                        let typed_expr = typed.pop().unwrap();

                        match op1 {
                            Op1::Add1 | Op1::Sub1 => {
                                FastTypedExpr::UnOp(FastExprType::Int, op1, Box::new(typed_expr))
                            }
                            Op1::Not => FastTypedExpr::UnOp(
                                typed_expr.extract_type(),
                                op1,
                                Box::new(typed_expr),
                            ),
                            Op1::Print => FastTypedExpr::UnOp(
                                typed_expr.extract_type(),
                                op1,
                                Box::new(typed_expr),
                            ),
                        }
                    }
                    .check_type(expected)
                }))
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

                StepResult::Nonterminal(StackState::new(
                    vec![wrapped_new_value],
                    move |mut typed: Vec<FastTypedExpr<'a>>, _| {
                        {
                            if typed.len() != 1 {
                                panic!("bad typed len")
                            }
                            let t1_prime = typed.pop().unwrap();

                            FastTypedExpr::Set(t1, name, Box::new(t1_prime))
                        }
                        .check_type(expected)
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

                    let mut b_vec = vec![*b];

                    StepResult::Nonterminal(StackState::new_plus(
                        vec![a_wrapped],
                        move |mut typed, _| {
                            {
                                assert!(typed.len() == 2);
                                let b_typed: FastTypedExpr = typed.pop().unwrap();
                                let a_typed = typed.pop().unwrap();
                                FastTypedExpr::BinOp(
                                    FastExprType::Bool,
                                    op2,
                                    Box::new(a_typed),
                                    Box::new(b_typed),
                                )
                            }
                            .check_type(expected)
                        },
                        move |typed, _state| match b_vec.pop() {
                            Some(b) => Some(WrappedFastExpr {
                                fast_expr: b,
                                struct_hm: self.struct_hm,
                                expected_type: ExpectedType::Specific(typed.extract_type()),
                                type_bindings: self.type_bindings.clone(),
                                function_sigs: self.function_sigs,
                                struct_sigs: self.struct_sigs,
                                allow_input: self.allow_input,
                            }),
                            None => None,
                        },
                    ))
                }

                op2 => {
                    let (a_exp_type, b_exp_type, expr_type) = match op2 {
                        Op2::Plus | Op2::Minus | Op2::Times => {
                            (FastExprType::Int, FastExprType::Int, FastExprType::Int)
                        }
                        Op2::Or | Op2::And => {
                            (FastExprType::Bool, FastExprType::Bool, FastExprType::Bool)
                        }
                        Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                            (FastExprType::Int, FastExprType::Int, FastExprType::Bool)
                        }
                        Op2::Equal => unreachable!(),
                    };
                    let (a_fst_typ, b_fst_typ, expr_type) = (a_exp_type, b_exp_type, expr_type);

                    StepResult::Nonterminal(StackState::new(
                        vec![
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
                        ],
                        move |mut typed, _| {
                            {
                                assert!(typed.len() == 2);
                                let b_typed = typed.pop().unwrap();
                                let a_typed = typed.pop().unwrap();
                                FastTypedExpr::BinOp(
                                    expr_type,
                                    op2,
                                    Box::new(a_typed),
                                    Box::new(b_typed),
                                )
                            }
                            .check_type(expected)
                        },
                    ))
                }
            },

            FastExpr::If(cond, val_if_true, val_if_false) => {
                let mut untyped = vec![*val_if_true, *val_if_false];
                untyped.reverse(); // so that first pop gives us val_if_true, second gives ups val_if_false

                StepResult::Nonterminal(StackState::new_plus(
                    vec![WrappedFastExpr {
                        fast_expr: *cond,
                        struct_hm: self.struct_hm,
                        // cond must typecheck to bool
                        expected_type: ExpectedType::Specific(FastExprType::Bool),
                        type_bindings: self.type_bindings.clone(),
                        function_sigs: self.function_sigs,
                        struct_sigs: self.struct_sigs,
                        allow_input: self.allow_input,
                    }],
                    move |mut typed, _| {
                        {
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
                        .check_type(expected)
                    },
                    move |typed: &FastTypedExpr, _: &mut TypeCheckState| {
                        match untyped.pop() {
                            None => None, // done (will construct)
                            Some(untyped_expr) => {
                                let expected_type = if untyped.len() == 1 {
                                    // first branch can be any type
                                    ExpectedType::Any
                                } else {
                                    assert!(untyped.len() == 0);
                                    // second branch must match first
                                    ExpectedType::Specific(typed.extract_type())
                                };
                                Some(WrappedFastExpr {
                                    fast_expr: untyped_expr,
                                    struct_hm: self.struct_hm,
                                    expected_type,
                                    type_bindings: self.type_bindings.clone(),
                                    function_sigs: self.function_sigs,
                                    struct_sigs: self.struct_sigs,
                                    allow_input: self.allow_input,
                                })
                            }
                        }
                    },
                ))
            }

            FastExpr::RepeatUntil(body, stop_cond) => StepResult::Nonterminal(StackState::new(
                vec![
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
                ],
                move |mut typed, _| {
                    {
                        assert_eq!(typed.len(), 2);
                        let typed_body: FastTypedExpr = typed.pop().unwrap();
                        let typed_stop_cond = typed.pop().unwrap();
                        FastTypedExpr::RepeatUntil(
                            typed_body.extract_type(),
                            Box::new(typed_body),
                            Box::new(typed_stop_cond),
                        )
                    }
                    .check_type(expected)
                },
            )),

            FastExpr::Block(expns) => {
                if expns.is_empty() {
                    panic!("Invalid: Block is Empty") // TODO CHANGE TO UNIT
                }
                let wrapped: Vec<_> = expns
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

                let wrapped_len = wrapped.len();

                StepResult::Nonterminal(StackState::new(
                    wrapped,
                    move |typed: Vec<FastTypedExpr>, _| {
                        {
                            assert_eq!(wrapped_len, typed.len());

                            // type of a block is type of last, or unit if empty
                            let block_type = if typed.len() == 0 {
                                FastExprType::Unit
                            } else {
                                typed.last().unwrap().extract_type()
                            };

                            FastTypedExpr::Block(block_type, typed)
                        }
                        .check_type(expected)
                    },
                ))
            }

            FastExpr::Call(fn_name_or_ptr, mut arguments) => {
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

                let mut parsed_args_expected_types: Vec<(FastExprType, FastExpr)> = vec![];
                let mut ptr_checked = false;

                StepResult::Nonterminal(StackState::new_plus(
                    vec![wrapped_name_or_ptr],
                    move |mut typed, state_plus: &mut TypeCheckState| {
                        {
                            assert!(typed.len() == arg_len + 1); // all args + fn ptr
                            let typed_fun_ptr: FastTypedExpr = typed.remove(0); // TODO POP FRONT

                            // can clean this up; and we never expect to need to clone here, as when creating the function type we should've ingested the ret type
                            let fast_ret_type =
                                match state_plus.to_expr_type_ref(typed_fun_ptr.extract_type()) {
                                    Some(ExprType::FunctionPointer(_, ret_type)) => {
                                        match state_plus.to_fexpr_t_opt(&ret_type) {
                                            Some(t) => t,
                                            None => unreachable!(),
                                        }
                                    }
                                    _ => unreachable!("should have panicked earlier"),
                                };
                            // let fast_ret_type = state_plus.ingest(ret_type).0; // TODO CLONE

                            FastTypedExpr::Call(fast_ret_type, Box::new(typed_fun_ptr), typed)
                        }
                        .check_type(expected)
                    },
                    move |produced, state: &mut TypeCheckState| {
                        // the first produced is the function pointer, which dictates the type and number of arguments
                        if !ptr_checked {
                            let arg_types = match state.to_expr_type_ref(produced.extract_type()) {
                                Some(ExprType::FunctionPointer(arg_types, _ret_type)) => {
                                    arg_types.clone()
                                }
                                _ => panic!("not function pointe"),
                            };

                            if arg_types.len() != arg_len {
                                panic!("wrong number of args")
                            }

                            parsed_args_expected_types = arg_types
                                .into_iter()
                                .map(|x| state.ingest(x).0)
                                .zip(std::mem::take(&mut arguments))
                                .collect();

                            parsed_args_expected_types.reverse(); // so that popping takes in order

                            ptr_checked = true;
                        }

                        match parsed_args_expected_types.pop() {
                            None => None, // done type checking

                            // typecheck next argument
                            Some((expect, to_check)) => Some(WrappedFastExpr {
                                fast_expr: to_check,
                                struct_hm: self.struct_hm,
                                expected_type: ExpectedType::Specific(expect),
                                type_bindings: self.type_bindings.clone(),
                                function_sigs: self.function_sigs,
                                struct_sigs: self.struct_sigs,
                                allow_input: self.allow_input,
                            }),
                        }
                    },
                ))
            }

            FastExpr::Update(pointer, field_name, new_value) => {
                let mut parse_next = vec![*new_value];
                StepResult::Nonterminal(StackState::new_plus(
                    vec![
                        self.split_child(*pointer, ExpectedType::Any), // must check that it has pointer type
                    ],
                    move |mut typed, _state| {
                        {
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
                        .check_type(expected)
                    },
                    move |typed: &FastTypedExpr<'a>, state: &mut TypeCheckState| {
                        match parse_next.pop() {
                            Some(new_value) => {
                                // check the pointer type
                                let pointed_struct_enum =
                                    match state.to_expr_type_ref(typed.extract_type()) {
                                        Some(ExprType::StructPointer(struct_enum)) => *struct_enum,
                                        _ => panic!(
                                            "Invalid: Update on non-pointer-typed expression: {:?}",
                                            "todo" // pointer
                                        ),
                                    };

                                // the kind of struct it points to must have a corresponding field
                                let pointed_struct_name =
                                    match struct_type_enum_to_name(pointed_struct_enum) {
                                        Some(s) => s,
                                        None => panic!(
                                            // type enum doesn't exist; should never happen
                                            "Unexpected: Update with non-existent struct with type enumeration: {}",
                                            pointed_struct_enum
                                        ),
                                    };

                                let struct_sig =
                                    match self.struct_sigs.get(pointed_struct_name.as_str()) {
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

                                // parse the new value
                                Some(self.split_child(
                                    new_value,
                                    ExpectedType::Specific(state.ingest(expected_type).0),
                                ))
                            }
                            None => None,
                        }
                    },
                ))
            }

            FastExpr::Lookup(pointer, field_name) => {
                StepResult::Nonterminal(StackState::new(
                    vec![
                        self.split_child(*pointer, ExpectedType::Any), // must check
                    ],
                    move |mut typed: Vec<FastTypedExpr>, state: &mut TypeCheckState| {
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

                        let struct_sig = match self.struct_sigs.get(pointed_struct_name.as_str()) {
                            Some(sig) => sig,
                            None => panic!(
                                "Unexpected: Lookup with non-existent struct: {}",
                                pointed_struct_name
                            ),
                        };

                        let expected_type = match struct_sig_type_of(struct_sig, &field_name) {
                        Some(expr_type) => expr_type,
                        None => panic!("Invalid: Lookup nonexistent field {field_name} in struct {pointed_struct_name}"),
                        };

                        let fast_expected_type = state.ingest(expected_type).0;

                        FastTypedExpr::Lookup(
                            fast_expected_type,
                            Box::new(typed_pointer),
                            field_name,
                        )
                    }.check_type(expected)
                    },
                ))
            }

            FastExpr::ArrayLookup(arr, ind) => StepResult::Nonterminal(StackState::new(
                vec![
                    self.split_child(*arr, ExpectedType::Any),
                    self.split_child(*ind, ExpectedType::Specific(FastExprType::Int)),
                ],
                move |mut typed, state: &mut TypeCheckState| {
                    {
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

                        FastTypedExpr::ArrayLookup(
                            elem_t_fexpr,
                            Box::new(typed_arr),
                            Box::new(typed_ind),
                        )
                    }
                    .check_type(expected)
                },
            )),

            // NEW PLUS
            FastExpr::ArrayUpdate(arr, ind, new_val) => {
                let mut parse_later = vec![*new_val];
                let mut parsed_ptr = false;
                StepResult::Nonterminal(StackState::new_plus(
                    vec![
                        self.split_child(*arr, ExpectedType::Any),
                        self.split_child(*ind, ExpectedType::Specific(FastExprType::Int)),
                    ],
                    move |mut typed, new_state: &mut TypeCheckState| {
                        {
                            assert!(typed.len() == 3);
                            // when we produce a the new_val in consume(), it is parsed before ind, so ind is parsed last
                            let typed_ind = typed.pop().unwrap();
                            let typed_new_val: FastTypedExpr = typed.pop().unwrap();
                            let typed_arr_ptr = typed.pop().unwrap();

                            let elem_type =
                                match new_state.to_expr_type_ref(typed_arr_ptr.extract_type()) {
                                    Some(ExprType::Array(elem_type)) => &**elem_type,
                                    _ => panic!(
                                    "expected array lookup to operate on array type, not on {:?}",
                                    typed_arr_ptr
                                ),
                                };

                            let elem_t_fexpr = match new_state.to_fexpr_t_opt(elem_type) {
                                Some(t) => t,
                                None => new_state.ingest(elem_type.clone()).0,
                            };

                            FastTypedExpr::ArrayUpdate(
                                elem_t_fexpr,
                                Box::new(typed_arr_ptr),
                                Box::new(typed_ind),
                                Box::new(typed_new_val),
                            )
                        }
                        .check_type(expected)
                    },
                    move |typed: &FastTypedExpr<'a>, state: &mut TypeCheckState| {
                        if parsed_ptr {
                            return None;
                        }

                        let typed_arr = typed;
                        parsed_ptr = true;

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

                        Some(self.split_child(
                            parse_later.pop().unwrap(),
                            ExpectedType::Specific(elem_t_fexpr),
                        ))
                    },
                ))
            }

            FastExpr::ArrayAlloc(elem_t, len) => {
                let arr_t = ExprType::Array(Box::new(elem_t.clone()));
                let arr_t = state.ingest(arr_t).0;

                let elem_t = state.ingest(elem_t).0;
                validate_type_fast(&elem_t, self.struct_hm, state).unwrap();

                StepResult::Nonterminal(StackState::new(
                    vec![WrappedFastExpr {
                        fast_expr: *len,
                        struct_hm: self.struct_hm,
                        expected_type: ExpectedType::Specific(FastExprType::Int),
                        type_bindings: self.type_bindings,
                        function_sigs: self.function_sigs,
                        struct_sigs: self.struct_sigs,
                        allow_input: self.allow_input,
                    }],
                    move |mut typed, _state| {
                        {
                            assert!(typed.len() == 1);
                            let typed = typed.pop().unwrap();
                            FastTypedExpr::ArrayAlloc(arr_t, Box::new(typed))
                        }
                        .check_type(expected)
                    },
                ))
            }

            FastExpr::ArrayLen(arr) => StepResult::Nonterminal(StackState::new(
                vec![self.split_child_inherit(*arr, ExpectedType::Any)],
                move |mut typed: Vec<FastTypedExpr>, _state| {
                    {
                        assert!(typed.len() == 1);
                        let typed_arr = typed.pop().unwrap();
                        FastTypedExpr::ArrayLen(FastExprType::Int, Box::new(typed_arr))
                    }
                    .check_type(expected)
                },
            )),
            FastExpr::__INTERNAL => unreachable!(),

            FastExpr::Let(mut bindings, finally) => {
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

                bindings.reverse(); // now the earliest binding to parse is back
                let (ids, mut binding_exprs): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();

                let mut cloned_ids = ids.clone(); // these are reversed, so pop from back

                let parse_first = binding_exprs.pop().unwrap();
                let tc_first_wrapped: WrappedFastExpr<'a, 'b> = WrappedFastExpr {
                    fast_expr: parse_first,
                    struct_hm: self.struct_hm,
                    expected_type: ExpectedType::Any, // variables can have any type
                    type_bindings: self.type_bindings.clone(),
                    function_sigs: self.function_sigs,
                    struct_sigs: self.struct_sigs,
                    allow_input: self.allow_input,
                };

                let mut finally_vec = Vec::new();
                finally_vec.push(finally);

                // holds the most updated version the type bindings vector
                let mut type_binding_vec = Vec::new();
                type_binding_vec.push(self.type_bindings);

                // NEW PLUS

                // let is a bit more complicated; when we parse the ith binding, we need to update the values we pass when parsing the i+1 binding.
                // we need to use the consume function to do this: when consuming a binding, produce the next unparsed (wrapped) binding with updated arguments
                StepResult::Nonterminal(StackState::new_plus(
                    vec![tc_first_wrapped],
                    move |mut typed: Vec<FastTypedExpr<'a>>, _| {
                        {
                            assert!(typed.len() == num_bindings + 1); // bindings plus body expression
                                                                      // get body expression
                            let body_texpr = typed.pop().unwrap();
                            let typed_bindings: Vec<_> = ids.into_iter().rev().zip(typed).collect();
                            FastTypedExpr::Let(
                                body_texpr.extract_type(),
                                typed_bindings,
                                Box::new(body_texpr),
                            )
                        }
                        .check_type(expected)
                    },
                    move |temp, _passed_state| {
                        if let Some(id) = cloned_ids.pop() {
                            let fast_expr_type = temp.extract_type();

                            let _ = &mut type_binding_vec[0].insert(id, fast_expr_type);
                        }

                        // try to parse the next binding
                        if let Some(next_binding) = binding_exprs.pop() {
                            // parse the next binding with updated type map
                            return Some(WrappedFastExpr {
                                fast_expr: next_binding,
                                struct_hm: self.struct_hm,
                                expected_type: ExpectedType::Any, // variable can have any type
                                type_bindings: type_binding_vec[0].clone(),
                                function_sigs: self.function_sigs,
                                struct_sigs: self.struct_sigs,
                                allow_input: self.allow_input,
                            });
                        };

                        // try to parse the let body once we've typecheck all bindings
                        match finally_vec.pop() {
                            Some(body) => {
                                let type_bindings = type_binding_vec.pop().unwrap();
                                Some(WrappedFastExpr {
                                    fast_expr: *body,
                                    struct_hm: self.struct_hm,
                                    expected_type: ExpectedType::Any, // let can have any type
                                    type_bindings,
                                    function_sigs: self.function_sigs,
                                    struct_sigs: self.struct_sigs,
                                    allow_input: self.allow_input,
                                })
                            }
                            None => None, // already typecheck body; done
                        }
                    },
                ))
            }
        }
    }
}

/* Prog / Function / Struct Typechecking */

impl<'a> FastUserFunction<'a> {
    fn typecheck(
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
        let type_checked_body = IterativeStack::new(type_set).iterate(wrapped_body);

        FastTypedFunction::Fun(
            name,
            FastFunSignature::Sig(ret_type, param_types),
            type_checked_body,
        )
    }
}

impl<'a> FastProg<'a> {
    pub fn typecheck(self) -> FastTypedProg<'a> {
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
                f.typecheck(
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

        let typed_body: FastTypedExpr<'a> =
            IterativeStack::new(&mut type_map).iterate(wrapped_body);

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
