use im::HashMap;

use crate::{
    semantics::{struct_name_to_type_enum, struct_type_enum_to_name},
    structs::*,
};

fn validate_type<'a>(
    expr_type: &ExprType,
    struct_enum_map: &HashMap<&'a str, i32>,
) -> Result<(), String> {
    let mut curr_type = expr_type;
    loop {
        match curr_type {
            // basic types
            ExprType::Int | ExprType::Bool | ExprType::Unit => return Ok(()),

            // array: validate elem_t
            ExprType::Array(inner_type) => {
                curr_type = inner_type;
                continue;
            }

            // struct pointer: validate declared struct
            ExprType::StructPointer(pointed_struct_enum) => {
                return {
                    match struct_type_enum_to_name(*pointed_struct_enum) {
                        None => Err(format!("Unrecognized struct enum {pointed_struct_enum}")),

                        Some(pointed_struct_name) => {
                            match struct_enum_map.get(pointed_struct_name.as_str()) {
                                None => {
                                    Err(format!("Unrecognized struct name: {pointed_struct_name}"))
                                }
                                Some(&lookup_res) => {
                                    if lookup_res != *pointed_struct_enum {
                                        return Err(format!("Unexpected: mismatch in structure enum {pointed_struct_enum} {lookup_res}"));
                                    }
                                    return Ok(());
                                }
                            }
                        }
                    }
                }
            }
            ExprType::FunctionPointer(vec, ret_type) => {
                for field_type in vec {
                    if let Err(s) = validate_type(field_type, struct_enum_map) {
                        return Err(s);
                    };
                }
                return validate_type(ret_type, struct_enum_map);
            }
        }
    }
}

/// Given a struct signature, lookup the type associated with the field with name field_name; or None if no such field exists
fn struct_sig_type_of<'a>(
    struct_sig: &FastStructSignature,
    field_name: &'a str,
) -> Option<ExprType> {
    let FastStructSignature::Sig(field_names) = struct_sig;
    for (field_type, name) in field_names {
        if *name == field_name {
            return Some(field_type.clone());
        }
    }
    None
}

impl<'a> FastExpr<'a> {
    fn typecheck(
        self,
        struct_hm: &HashMap<&str, i32>,
        expected_type: Option<&ExprType>, // None for any type, or a specific type
        type_bindings: im::HashMap<&'a str, ExprType>,
        function_sigs: im::HashMap<&'a str, FastFunSignature>,
        struct_sigs: im::HashMap<&'a str, FastStructSignature>,
        allow_input: bool,
    ) -> FastTypedExpr<'a> {
        let tchecked_exr = match self {
            FastExpr::Input => {
                if !allow_input {
                    panic!("Invalid: Input is not an Int")
                }
                FastTypedExpr::Input
            }

            FastExpr::Boolean(b) => FastTypedExpr::Boolean(b),

            FastExpr::Id(id) => match function_sigs.get(&id) {
                // first, check for function names (which resolve to function pointers)
                Some(FastFunSignature::Sig(ret_type, param_types)) => FastTypedExpr::FunName(
                    ExprType::FunctionPointer(
                        param_types
                            .into_iter()
                            .map(
                                |(field_type, _field_name)| field_type.to_owned(), // TODO CLONE
                            )
                            .collect(),
                        Box::new(ret_type.clone()), // TODO CLONE
                    ),
                    id,
                ),

                // next, check for variables
                None => match type_bindings.get(id) {
                    None => panic!("Invalid: Unbound variable identifier {}", id),
                    Some(t) => FastTypedExpr::Id(t.clone(), id), // TODO CLONE
                },
            },

            FastExpr::Number(n) => FastTypedExpr::Number(n),

            FastExpr::UnOp(op1, expr) => {
                let expected_type = match op1 {
                    Op1::Add1 | Op1::Sub1 => Some(ExprType::Int),
                    Op1::Not => Some(ExprType::Bool),
                    Op1::Print => None,
                };

                let typed_expr = expr.typecheck(
                    struct_hm,
                    expected_type.as_ref(),
                    type_bindings,
                    function_sigs,
                    struct_sigs,
                    allow_input,
                );

                match op1 {
                    Op1::Add1 | Op1::Sub1 => {
                        FastTypedExpr::UnOp(ExprType::Int, op1, Box::new(typed_expr))
                    }
                    Op1::Not => {
                        FastTypedExpr::UnOp(typed_expr.extract_type(), op1, Box::new(typed_expr))
                    }
                    Op1::Print => {
                        FastTypedExpr::UnOp(typed_expr.extract_type(), op1, Box::new(typed_expr))
                    }
                }
            }

            FastExpr::Set(name, new_value) => {
                // fails if the name isn't in scope
                let t1 = match type_bindings.get(name) {
                    Some(t1) => t1,
                    None => panic!("Invalid: Unbound variable identifier {}", name),
                };

                // can only "set" a variable to the same type within the current scope
                let expected_type = Some(t1);
                let t1_prime = new_value.typecheck(
                    struct_hm,
                    expected_type,
                    type_bindings.clone(),
                    function_sigs,
                    struct_sigs,
                    allow_input,
                );
                FastTypedExpr::Set(
                    t1.clone(), // TODO CLONE
                    name,
                    Box::new(t1_prime),
                )
            }

            FastExpr::Let(bindings, finally) => {
                let mut curr_let_binding = type_bindings;
                let mut in_this_let: im::HashSet<&'a str> = im::HashSet::new();
                let mut bindings_typed_exnr: Vec<(&'a str, FastTypedExpr)> = Vec::new();

                for (id, exp) in bindings {
                    // check for duplicates
                    match in_this_let.insert(id) {
                        // TODO CLONE
                        None => (),
                        Some(_) => panic!("Duplicate binding"),
                    };

                    // typecheck the expression
                    // panics if if doesn't typecheck
                    let typed_expr = exp.typecheck(
                        struct_hm,
                        None, // variable can have any type
                        curr_let_binding.clone(),
                        function_sigs.clone(),
                        struct_sigs.clone(),
                        allow_input,
                    );

                    // bind id to that type, allowing shadowing of different types
                    curr_let_binding.insert(id, typed_expr.extract_type());

                    // build binding part of the typed expression
                    bindings_typed_exnr.push((id, typed_expr));
                }

                // evaluate the type of the final expression after all the bindings
                let final_typed_expr = finally.typecheck(
                    struct_hm,
                    None,
                    curr_let_binding,
                    function_sigs,
                    struct_sigs,
                    allow_input,
                );

                FastTypedExpr::Let(
                    final_typed_expr.extract_type(),
                    bindings_typed_exnr,
                    Box::new(final_typed_expr),
                )
            }

            FastExpr::BinOp(op2, a, b) => match op2 {
                // t * t -> bool
                Op2::Equal => {
                    let a_typed_exprn = a.typecheck(
                        struct_hm,
                        None, // t is free
                        type_bindings.clone(),
                        function_sigs.clone(),
                        struct_sigs.clone(),
                        allow_input,
                    );
                    let a_type = a_typed_exprn.extract_type();

                    let b_typed_exprn = b.typecheck(
                        struct_hm,
                        Some(a_type).as_ref(), // must match a's type
                        type_bindings.clone(),
                        function_sigs.clone(),
                        struct_sigs.clone(),
                        allow_input,
                    );

                    FastTypedExpr::BinOp(
                        ExprType::Bool,
                        op2,
                        Box::new(a_typed_exprn),
                        Box::new(b_typed_exprn),
                    )
                }

                // t1 * t1 -> t2
                op2 => {
                    let (a_exp_type, b_exp_type, expr_type) = match op2 {
                        Op2::Plus | Op2::Minus | Op2::Times => {
                            (ExprType::Int, ExprType::Int, ExprType::Int)
                        }
                        Op2::Or | Op2::And => (ExprType::Bool, ExprType::Bool, ExprType::Bool),
                        Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                            (ExprType::Int, ExprType::Int, ExprType::Bool)
                        }
                        Op2::Equal => unreachable!(),
                    };

                    let a_typed_exprn = a.typecheck(
                        struct_hm,
                        Some(&a_exp_type),
                        type_bindings.clone(),
                        function_sigs.clone(),
                        struct_sigs.clone(),
                        allow_input,
                    );

                    let b_typed_exprn = b.typecheck(
                        struct_hm,
                        Some(&b_exp_type),
                        type_bindings.clone(),
                        function_sigs.clone(),
                        struct_sigs.clone(),
                        allow_input,
                    );

                    FastTypedExpr::BinOp(
                        expr_type,
                        op2,
                        Box::new(a_typed_exprn),
                        Box::new(b_typed_exprn),
                    )
                }
            },

            // bool * t2 * t2 => t2
            FastExpr::If(cond, val_if_true, val_if_false) => {
                // cond should typecheck to bool
                let typed_cond = cond.typecheck(
                    struct_hm,
                    Some(ExprType::Bool).as_ref(),
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                let typed_if_true = val_if_true.typecheck(
                    struct_hm,
                    None, // t2 can be any
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                let type_if_true = typed_if_true.extract_type();

                let typed_if_false = val_if_false.typecheck(
                    struct_hm,
                    Some(&type_if_true), // t2 now constrained
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                FastTypedExpr::If(
                    type_if_true,
                    Box::new(typed_cond),
                    Box::new(typed_if_true),
                    Box::new(typed_if_false),
                )
            }

            // t1 * t2 => t1
            FastExpr::RepeatUntil(body, stop_cond) => {
                let typed_stop_cond = stop_cond.typecheck(
                    struct_hm,
                    Some(ExprType::Bool).as_ref(), // stop_cond must be a bool
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                // repeat-until evaluates to the body (once stop_cond is true)
                let typed_body = body.typecheck(
                    struct_hm,
                    None,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                FastTypedExpr::RepeatUntil(
                    typed_body.extract_type(),
                    Box::new(typed_body),
                    Box::new(typed_stop_cond),
                )
            }

            FastExpr::Block(expns) => {
                if expns.is_empty() {
                    panic!("Invalid: Block is Empty") // TODO CHANGE TO UNIT
                }
                let mut block_typed_exprn: Vec<FastTypedExpr> = Vec::new();

                for expr in expns {
                    // typecheck each expression in the block
                    let typed_exprn = expr.typecheck(
                        struct_hm,
                        None,
                        type_bindings.clone(),
                        function_sigs.clone(),
                        struct_sigs.clone(),
                        allow_input,
                    );
                    block_typed_exprn.push(typed_exprn);
                }

                // block evaluates to the type of the last expression
                let final_type = block_typed_exprn.last().unwrap().extract_type();

                FastTypedExpr::Block(final_type, block_typed_exprn)
            }

            FastExpr::Call(fn_name_or_ptr, arguments) => {
                // get the [unnamed parameter] function signature
                let typed_name_or_ptr = fn_name_or_ptr.typecheck(
                    struct_hm,
                    None, // needs to be a function pointer... of what we don't know
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                // confirm that the expression type is indeed function pointer
                let extracted_type = typed_name_or_ptr.extract_type();
                let (param_types, ret_type) = match extracted_type {
                    ExprType::FunctionPointer(arg_types, ret_type) => (arg_types, ret_type),
                    _ => panic!(
                        "Invalid Call: {:?} is not a function pointer type [in {:?}]",
                        extracted_type, typed_name_or_ptr
                    ),
                };

                let param_types: Vec<&ExprType> = param_types.iter().collect();

                // check that arguments are well typed, and agree with function sig
                let typed_args: Vec<FastTypedExpr> = arguments
                    .into_iter()
                    .zip(param_types)
                    .map(|(arg_exp, expected_type)| {
                        arg_exp.typecheck(
                            struct_hm,
                            Some(expected_type),
                            type_bindings.clone(),
                            function_sigs.clone(),
                            struct_sigs.clone(),
                            allow_input,
                        )
                    })
                    .collect();

                // Check function
                FastTypedExpr::Call(*ret_type, Box::new(typed_name_or_ptr), typed_args)
            }

            FastExpr::Null(struct_name) => {
                // check that struct type actually exists (and is well typed)
                if !struct_sigs.contains_key(&struct_name) {
                    panic!(
                        "Invalid: null pointer to unknown struct type: {}",
                        struct_name
                    )
                } // struct_name is valid
                let struct_enum = struct_name_to_type_enum(&struct_name.to_string()); // TODO STRING
                FastTypedExpr::Null(ExprType::StructPointer(struct_enum))
            }
            FastExpr::Alloc(struct_name) => {
                // check that struct type actually exists (and is well typed)
                if !struct_sigs.contains_key(&struct_name) {
                    panic!("Invalid: alloc of unknown struct type: {}", struct_name)
                } // struct_name is valid
                let struct_enum = struct_name_to_type_enum(&struct_name.to_string());
                FastTypedExpr::Alloc(ExprType::StructPointer(struct_enum))
            }
            FastExpr::Update(pointer, field_name, new_value) => {
                // first expression must be a pointer type
                let pointer_typed_expr = pointer.typecheck(
                    struct_hm,
                    None, // must check type after typechecking
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                let pointed_struct_enum = match pointer_typed_expr.extract_type() {
                    ExprType::StructPointer(struct_enum) => struct_enum,
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

                let struct_sig = match struct_sigs.get(pointed_struct_name.as_str()) {
                    Some(sig) => sig, // struct name not in struct_sigs; shouldn't happen
                    None => panic!(
                        "Unexpected: Update with non-existent struct signature: {}",
                        pointed_struct_name
                    ),
                };

                // this is an error which could actually happen
                let expected_type = match struct_sig_type_of(struct_sig, &field_name.to_string()) {
                    // TODO STRING
                    Some(expr_type) => expr_type,
                    None => panic!(
                    "Invalid: tried to update field name {} of struct {} which has no such field",
                    pointed_struct_name, field_name
                ),
                };

                // get the type of the new value
                let update_typed_expr = new_value.typecheck(
                    struct_hm,
                    Some(&expected_type),
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                FastTypedExpr::Update(
                    expected_type,
                    Box::new(pointer_typed_expr),
                    field_name,
                    Box::new(update_typed_expr),
                )
            }
            FastExpr::Lookup(pointer, field_name) => {
                // first expression must be a pointer type
                let pointer_typed_expr = pointer.typecheck(
                    struct_hm,
                    None, // must check
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                let pointed_struct_enum = match pointer_typed_expr.extract_type() {
                    ExprType::StructPointer(struct_enum) => struct_enum,
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

                let expected_type = match struct_sig_type_of(struct_sig, &field_name.to_string()) { // TODO STRING
                Some(expr_type) => expr_type,
                None => panic!("Invalid: Lookup nonexistent field {field_name} in struct {pointed_struct_name}"),
                };

                FastTypedExpr::Lookup(expected_type, Box::new(pointer_typed_expr), field_name)
            }
            FastExpr::Unit => FastTypedExpr::Unit,
            FastExpr::FunName(fun_name) => match function_sigs.get(&fun_name) {
                // check global functions first
                Some(FastFunSignature::Sig(return_type, param_types)) => {
                    let param_types: Vec<ExprType> = param_types
                        .into_iter()
                        .map(|(param_type, _param_name)| param_type.to_owned()) // TODO CLONE
                        .collect();

                    FastTypedExpr::FunName(
                        ExprType::FunctionPointer(param_types, Box::new(return_type.clone())),
                        fun_name,
                    )
                }

                None => {
                    // variables next
                    match type_bindings.get(fun_name) {
                        Some(ExprType::FunctionPointer(arg_types, ret_type)) => FastTypedExpr::Id(
                            ExprType::FunctionPointer(arg_types.to_owned(), ret_type.to_owned()),
                            fun_name,
                        ),
                        _ => panic!(
                            "Invalid: Called function {:?}, which doesn't exist",
                            fun_name
                        ),
                    }
                }
            },
            FastExpr::ArrayLookup(arr, ind) => {
                let typed_arr = arr.typecheck(
                    struct_hm,
                    None,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                let elem_type = match typed_arr.extract_type() {
                    ExprType::Array(elem_type) => *elem_type,
                    _ => panic!(
                        "expected array lookup to operate on array type, not on {:?}",
                        typed_arr
                    ),
                };

                let typed_ind = ind.typecheck(
                    struct_hm,
                    Some(ExprType::Int).as_ref(),
                    type_bindings,
                    function_sigs,
                    struct_sigs,
                    allow_input,
                );

                FastTypedExpr::ArrayLookup(elem_type, Box::new(typed_arr), Box::new(typed_ind))
            }
            FastExpr::ArrayUpdate(arr, ind, new_val) => {
                let typed_arr = arr.typecheck(
                    struct_hm,
                    None,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input.clone(),
                );
                let elem_type = match typed_arr.extract_type() {
                    ExprType::Array(elem_type) => *elem_type,
                    _ => panic!(
                        "expected array lookup to operate on array type, not on {:?}",
                        typed_arr
                    ),
                };
                let typed_ind = ind.typecheck(
                    struct_hm,
                    Some(&ExprType::Int),
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input.clone(),
                );
                let typed_new_val = new_val.typecheck(
                    struct_hm,
                    Some(&elem_type),
                    type_bindings,
                    function_sigs,
                    struct_sigs,
                    allow_input,
                );

                FastTypedExpr::ArrayUpdate(
                    elem_type,
                    Box::new(typed_arr),
                    Box::new(typed_ind),
                    Box::new(typed_new_val),
                )
            }
            FastExpr::ArrayAlloc(elem_t, len) => {
                validate_type(&elem_t, struct_hm).unwrap();

                let parsed_len = len.typecheck(
                    struct_hm,
                    Some(&ExprType::Int),
                    type_bindings,
                    function_sigs,
                    struct_sigs,
                    allow_input,
                );

                FastTypedExpr::ArrayAlloc(elem_t, Box::new(parsed_len))
            }
            FastExpr::ArrayLen(arr) => {
                let typed_arr = arr.typecheck(
                    struct_hm,
                    None,
                    type_bindings,
                    function_sigs,
                    struct_sigs,
                    allow_input,
                );
                match typed_arr.extract_type() {
                    ExprType::Array(_) => (),
                    _ => panic!("arr_len called on expression which isn't an array"),
                }

                FastTypedExpr::ArrayLen(ExprType::Int, Box::new(typed_arr))
            }
        };

        if let Some(expected_type) = expected_type {
            let got_type = tchecked_exr.extract_type();
            if got_type != *expected_type {
                panic!("Invalid: Type mismatch; expected expression {:?} to have type {:?} but it has type {:?}", tchecked_exr, expected_type, got_type)
            }
        }

        tchecked_exr
    }
}

impl<'a> FastUserFunction<'a> {
    fn typecheck(
        self,
        struct_sigs: im::HashMap<&'a str, FastStructSignature>,
        function_sigs: im::HashMap<&'a str, FastFunSignature>,
        struct_hm: &HashMap<&'a str, i32>,
    ) -> FastTypedFunction<'a> {
        let FastUserFunction::UserFun(name, FastFunSignature::Sig(ret_type, param_types), body) =
            self;

        // insert args into type bindings
        let mut type_bindings: HashMap<&'a str, ExprType> = im::HashMap::new();
        for (param_type, param_name) in param_types.iter() {
            match type_bindings.get(param_name) {
                Some(_) => panic!("Duplicate Argument"),
                None => type_bindings.insert(param_name, param_type.clone()), // TODO CLONE
            };
        }

        // get actual return type of body
        let type_checked_body = body.typecheck(
            struct_hm,
            Some(&ret_type), // must match signature type
            type_bindings,
            function_sigs.clone(),
            struct_sigs.clone(),
            false,
        );

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
        let mut struct_enum_map: HashMap<&'a str, i32> = HashMap::new();
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
        let mut struct_layouts: HashMap<&'a str, FastStructLayout> = im::HashMap::new();
        for FastUserStruct::UserStruct(struct_name, FastStructSignature::Sig(field_names)) in
            structs
        {
            let mut checked_struct_fields: Vec<(ExprType, &'a str)> = Vec::new();
            let mut struct_fields_names: HashMap<&'a str, i32> = im::HashMap::new();
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
        let mut function_sigs: im::HashMap<&'a str, FastFunSignature> = im::HashMap::new();
        for FastUserFunction::UserFun(name, function_sig, _) in functions.iter() {
            match function_sigs.get(name) {
                Some(_) => panic!("Duplicate Function Definition"),
                None => {
                    function_sigs.insert(name, function_sig.to_owned()); // TODO CLONE

                    let FastFunSignature::Sig(expr_type, vec) = &function_sig;
                    validate_type(expr_type, &struct_enum_map).unwrap();
                    for (v, _) in vec {
                        validate_type(v, &struct_enum_map).unwrap();
                    }
                }
            };
        }

        let typed_functions = functions
            .into_iter()
            .map(|f| {
                f.typecheck(
                    struct_type_map.clone(),
                    function_sigs.clone(),
                    &struct_enum_map,
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
        let typed_body = body.typecheck(
            &struct_enum_map,
            None, // program body can have any type
            im::HashMap::new(),
            function_sigs.clone(),
            struct_type_map.clone(),
            true,
        );
        FastTypedProg::Program(
            typed_body.extract_type(),
            struct_type_map,
            struct_layouts,
            typed_functions,
            typed_body,
        )
    }
}

impl<'a> FastTypedProg<'a> {
    pub fn to_slow(self) -> TypedProg {
        let FastTypedProg::Program(expr_type, hash_map, hash_map1, vec, fast_typed_expr) = self;
        let new_map = HashMap::from_iter(
            hash_map
                .into_iter()
                .map(|(key, val)| (key.to_string(), val.to_slow())),
        );

        let new_map1 = HashMap::from_iter(
            hash_map1
                .into_iter()
                .map(|(key, val)| (key.to_string(), val.to_slow())),
        );
        TypedProg::Program(
            expr_type,
            new_map,
            new_map1,
            vec.into_iter().map(|fte| fte.to_slow()).collect(),
            fast_typed_expr.to_slow(),
        )
    }
}

impl<'a> FastStructLayout<'a> {
    pub fn to_slow(self) -> StructLayout {
        let FastStructLayout::Layout(hash_map) = self;
        StructLayout::Layout(HashMap::from_iter(
            hash_map
                .into_iter()
                .map(|(name_str, offset)| (name_str.to_string(), offset)),
        ))
    }
}

impl<'a> FastStructSignature<'a> {
    pub fn to_slow(self) -> StructSignature {
        let FastStructSignature::Sig(vec) = self;
        StructSignature::Sig(
            vec.into_iter()
                .map(|(field_type, field_str)| (field_type, field_str.to_string()))
                .collect(),
        )
    }
}

impl<'a> FastTypedFunction<'a> {
    pub fn to_slow(self) -> TypedFunction {
        let FastTypedFunction::Fun(name_str, fast_fun_signature, fast_typed_expr) = self;
        TypedFunction::Fun(
            name_str.to_string(),
            fast_fun_signature.to_slow(),
            fast_typed_expr.to_slow(),
        )
    }
}

impl<'a> FastFunSignature<'a> {
    fn to_slow(self) -> FunSignature {
        let FastFunSignature::Sig(expr_type, vec) = self;
        FunSignature::Sig(
            expr_type,
            vec.into_iter()
                .map(|(expr_type, str)| (expr_type, str.to_string()))
                .collect(),
        )
    }
}

impl<'a> FastTypedExpr<'a> {
    pub fn to_slow(self) -> TypedExpr {
        match self {
            FastTypedExpr::Number(x) => TypedExpr::Number(x),
            FastTypedExpr::Boolean(b) => TypedExpr::Boolean(b),
            FastTypedExpr::Id(expr_type, name) => TypedExpr::Id(expr_type, name.to_string()),
            FastTypedExpr::Let(expr_type, vec, fast_typed_expr) => TypedExpr::Let(
                expr_type,
                vec.into_iter()
                    .map(|(s, fte)| (s.to_string(), fte.to_slow()))
                    .collect(),
                Box::new(fast_typed_expr.to_slow()),
            ),
            FastTypedExpr::UnOp(expr_type, op1, fast_typed_expr) => {
                TypedExpr::UnOp(expr_type, op1, Box::new(fast_typed_expr.to_slow()))
            }
            FastTypedExpr::BinOp(expr_type, op2, fast_typed_expr, fast_typed_expr1) => {
                TypedExpr::BinOp(
                    expr_type,
                    op2,
                    Box::new(fast_typed_expr.to_slow()),
                    Box::new(fast_typed_expr1.to_slow()),
                )
            }
            FastTypedExpr::If(expr_type, fast_typed_expr, fast_typed_expr1, fast_typed_expr2) => {
                TypedExpr::If(
                    expr_type,
                    Box::new(fast_typed_expr.to_slow()),
                    Box::new(fast_typed_expr1.to_slow()),
                    Box::new(fast_typed_expr2.to_slow()),
                )
            }
            FastTypedExpr::RepeatUntil(expr_type, fast_typed_expr, fast_typed_expr1) => {
                TypedExpr::RepeatUntil(
                    expr_type,
                    Box::new(fast_typed_expr.to_slow()),
                    Box::new(fast_typed_expr1.to_slow()),
                )
            }
            FastTypedExpr::Set(expr_type, s, fast_typed_expr) => TypedExpr::Set(
                expr_type,
                s.to_string(),
                Box::new(fast_typed_expr.to_slow()),
            ),
            FastTypedExpr::Block(expr_type, vec) => TypedExpr::Block(
                expr_type,
                vec.into_iter().map(|fte| fte.to_slow()).collect(),
            ),
            FastTypedExpr::FunName(expr_type, name) => {
                TypedExpr::FunName(expr_type, name.to_string())
            }
            FastTypedExpr::Call(expr_type, fast_typed_expr, vec) => TypedExpr::Call(
                expr_type,
                Box::new(fast_typed_expr.to_slow()),
                vec.into_iter().map(|fte| fte.to_slow()).collect(),
            ),
            FastTypedExpr::Input => TypedExpr::Input,
            FastTypedExpr::RDInput => TypedExpr::RDInput,
            FastTypedExpr::Null(expr_type) => TypedExpr::Null(expr_type),
            FastTypedExpr::Alloc(expr_type) => TypedExpr::Alloc(expr_type),
            FastTypedExpr::Update(expr_type, fast_typed_expr, name, fast_typed_expr1) => {
                TypedExpr::Update(
                    expr_type,
                    Box::new(fast_typed_expr.to_slow()),
                    name.to_string(),
                    Box::new(fast_typed_expr1.to_slow()),
                )
            }
            FastTypedExpr::Lookup(expr_type, fast_typed_expr, name) => TypedExpr::Lookup(
                expr_type,
                Box::new(fast_typed_expr.to_slow()),
                name.to_string(),
            ),
            FastTypedExpr::Unit => TypedExpr::Unit,
            FastTypedExpr::ArrayAlloc(expr_type, fast_typed_expr) => {
                TypedExpr::ArrayAlloc(expr_type, Box::new(fast_typed_expr.to_slow()))
            }
            FastTypedExpr::ArrayLookup(expr_type, fast_typed_expr, fast_typed_expr1) => {
                TypedExpr::ArrayLookup(
                    expr_type,
                    Box::new(fast_typed_expr.to_slow()),
                    Box::new(fast_typed_expr1.to_slow()),
                )
            }
            FastTypedExpr::ArrayUpdate(
                expr_type,
                fast_typed_expr,
                fast_typed_expr1,
                fast_typed_expr2,
            ) => TypedExpr::ArrayUpdate(
                expr_type,
                Box::new(fast_typed_expr.to_slow()),
                Box::new(fast_typed_expr1.to_slow()),
                Box::new(fast_typed_expr2.to_slow()),
            ),
            FastTypedExpr::ArrayLen(expr_type, fast_typed_expr) => {
                TypedExpr::ArrayLen(expr_type, Box::new(fast_typed_expr.to_slow()))
            }
        }
    }
}
