use im::HashMap;

use crate::{
    semantics::{struct_name_to_type_enum, struct_type_enum_to_name},
    structs::*,
};

fn validate_type(
    expr_type: &ExprType,
    struct_enum_map: &HashMap<String, i32>,
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
                            match struct_enum_map.get(&pointed_struct_name) {
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
fn struct_sig_type_of(struct_sig: &StructSignature, field_name: &String) -> Option<ExprType> {
    let StructSignature::Sig(field_names) = struct_sig;
    for (field_type, name) in field_names {
        if name == field_name {
            return Some(field_type.clone());
        }
    }
    None
}

impl Expr {
    fn typecheck(
        self,
        struct_hm: &HashMap<String, i32>,
        expected_type: Option<&ExprType>, // None for any type, or a specific type
        type_bindings: im::HashMap<String, ExprType>,
        function_sigs: im::HashMap<String, FunSignature>,
        struct_sigs: im::HashMap<String, StructSignature>,
        allow_input: bool,
    ) -> TypedExpr {
        let tchecked_exr = match self {
            Expr::Input => {
                if !allow_input {
                    panic!("Invalid: Input is not an Int")
                }
                TypedExpr::Input
            }

            Expr::Boolean(b) => TypedExpr::Boolean(b),

            Expr::Id(id) => match function_sigs.get(&id) {
                // first, check for function names (which resolve to function pointers)
                Some(FunSignature::Sig(ret_type, param_types)) => TypedExpr::FunName(
                    ExprType::FunctionPointer(
                        param_types
                            .into_iter()
                            .map(
                                |(field_type, _field_name)| field_type.to_owned(), // TODO CLONE
                            )
                            .collect(),
                        Box::new(ret_type.clone()), // TODO CLONE
                    ),
                    id.to_string(),
                ),

                // next, check for variables
                None => match type_bindings.get(&id) {
                    None => panic!("Invalid: Unbound variable identifier {}", id),
                    Some(t) => TypedExpr::Id(t.clone(), id), // TODO CLONE
                },
            },

            Expr::Number(n) => TypedExpr::Number(n),

            Expr::UnOp(op1, expr) => {
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
                        TypedExpr::UnOp(ExprType::Int, op1, Box::new(typed_expr))
                    }
                    Op1::Not => {
                        TypedExpr::UnOp(extract_type(&typed_expr), op1, Box::new(typed_expr))
                    }
                    Op1::Print => {
                        TypedExpr::UnOp(extract_type(&typed_expr), op1, Box::new(typed_expr))
                    }
                }
            }

            Expr::Set(name, new_value) => {
                // fails if the name isn't in scope
                let t1 = match type_bindings.get(&name) {
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
                TypedExpr::Set(
                    t1.clone(), // TODO CLONE
                    name,
                    Box::new(t1_prime),
                )
            }

            Expr::Let(bindings, finally) => {
                let mut curr_let_binding = type_bindings;
                let mut in_this_let: im::HashSet<String> = im::HashSet::new();
                let mut bindings_typed_exnr: Vec<(String, TypedExpr)> = Vec::new();

                for (id, exp) in bindings {
                    // check for duplicates
                    match in_this_let.insert(id.to_string()) {
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
                    curr_let_binding.insert(id.to_string(), extract_type(&typed_expr));

                    // build binding part of the typed expression
                    bindings_typed_exnr.push((id.to_string(), typed_expr));
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

                TypedExpr::Let(
                    extract_type(&final_typed_expr),
                    bindings_typed_exnr,
                    Box::new(final_typed_expr),
                )
            }

            Expr::BinOp(op2, a, b) => match op2 {
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
                    let a_type = extract_type(&a_typed_exprn);

                    let b_typed_exprn = b.typecheck(
                        struct_hm,
                        Some(a_type).as_ref(), // must match a's type
                        type_bindings.clone(),
                        function_sigs.clone(),
                        struct_sigs.clone(),
                        allow_input,
                    );

                    TypedExpr::BinOp(
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

                    TypedExpr::BinOp(
                        expr_type,
                        op2,
                        Box::new(a_typed_exprn),
                        Box::new(b_typed_exprn),
                    )
                }
            },

            // bool * t2 * t2 => t2
            Expr::If(cond, val_if_true, val_if_false) => {
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
                let type_if_true = extract_type(&typed_if_true);

                let typed_if_false = val_if_false.typecheck(
                    struct_hm,
                    Some(&type_if_true), // t2 now constrained
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                TypedExpr::If(
                    type_if_true,
                    Box::new(typed_cond),
                    Box::new(typed_if_true),
                    Box::new(typed_if_false),
                )
            }

            // t1 * t2 => t1
            Expr::RepeatUntil(body, stop_cond) => {
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

                TypedExpr::RepeatUntil(
                    extract_type(&typed_body),
                    Box::new(typed_body),
                    Box::new(typed_stop_cond),
                )
            }

            Expr::Block(expns) => {
                if expns.is_empty() {
                    panic!("Invalid: Block is Empty") // TODO CHANGE TO UNIT
                }
                let mut block_typed_exprn: Vec<TypedExpr> = Vec::new();

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
                let final_type = extract_type(&block_typed_exprn.last().unwrap());

                TypedExpr::Block(final_type, block_typed_exprn)
            }

            Expr::Call(fn_name_or_ptr, arguments) => {
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
                let extracted_type = extract_type(&typed_name_or_ptr);
                let (param_types, ret_type) = match extracted_type {
                    ExprType::FunctionPointer(arg_types, ret_type) => (arg_types, ret_type),
                    _ => panic!(
                        "Invalid Call: {:?} is not a function pointer type [in {:?}]",
                        extracted_type, typed_name_or_ptr
                    ),
                };

                let param_types: Vec<&ExprType> = param_types.iter().collect();

                // check that arguments are well typed, and agree with function sig
                let typed_args: Vec<TypedExpr> = arguments
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
                TypedExpr::Call(*ret_type, Box::new(typed_name_or_ptr), typed_args)
            }

            Expr::Null(struct_name) => {
                // check that struct type actually exists (and is well typed)
                if !struct_sigs.contains_key(&struct_name) {
                    panic!(
                        "Invalid: null pointer to unknown struct type: {}",
                        struct_name
                    )
                } // struct_name is valid
                let struct_enum = struct_name_to_type_enum(&struct_name);
                TypedExpr::Null(ExprType::StructPointer(struct_enum))
            }
            Expr::Alloc(struct_name) => {
                // check that struct type actually exists (and is well typed)
                if !struct_sigs.contains_key(&struct_name) {
                    panic!("Invalid: alloc of unknown struct type: {}", struct_name)
                } // struct_name is valid
                let struct_enum = struct_name_to_type_enum(&struct_name);
                TypedExpr::Alloc(ExprType::StructPointer(struct_enum))
            }
            Expr::Update(pointer, field_name, new_value) => {
                // first expression must be a pointer type
                let pointer_typed_expr = pointer.typecheck(
                    struct_hm,
                    None, // must check type after typechecking
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                let pointed_struct_enum = match extract_type(&pointer_typed_expr) {
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

                let struct_sig = match struct_sigs.get(&pointed_struct_name) {
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

                // get the type of the new value
                let update_typed_expr = new_value.typecheck(
                    struct_hm,
                    Some(&expected_type),
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                TypedExpr::Update(
                    expected_type,
                    Box::new(pointer_typed_expr),
                    field_name,
                    Box::new(update_typed_expr),
                )
            }
            Expr::Lookup(pointer, field_name) => {
                // first expression must be a pointer type
                let pointer_typed_expr = pointer.typecheck(
                    struct_hm,
                    None, // must check
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                let pointed_struct_enum = match extract_type(&pointer_typed_expr) {
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

                let struct_sig = match struct_sigs.get(&pointed_struct_name) {
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

                TypedExpr::Lookup(expected_type, Box::new(pointer_typed_expr), field_name)
            }
            Expr::Unit => TypedExpr::Unit,
            Expr::FunName(fun_name) => match function_sigs.get(&fun_name) {
                // check global functions first
                Some(FunSignature::Sig(return_type, param_types)) => {
                    let param_types: Vec<ExprType> = param_types
                        .into_iter()
                        .map(|(param_type, _param_name)| param_type.to_owned()) // TODO CLONE
                        .collect();

                    TypedExpr::FunName(
                        ExprType::FunctionPointer(param_types, Box::new(return_type.clone())),
                        fun_name.to_string(),
                    )
                }

                None => {
                    // variables next
                    match type_bindings.get(&fun_name) {
                        Some(ExprType::FunctionPointer(arg_types, ret_type)) => TypedExpr::Id(
                            ExprType::FunctionPointer(arg_types.to_owned(), ret_type.to_owned()),
                            fun_name.to_string(),
                        ),
                        _ => panic!(
                            "Invalid: Called function {:?}, which doesn't exist",
                            fun_name
                        ),
                    }
                }
            },
            Expr::ArrayLookup(arr, ind) => {
                let typed_arr = arr.typecheck(
                    struct_hm,
                    None,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                let elem_type = match extract_type(&typed_arr) {
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

                TypedExpr::ArrayLookup(elem_type, Box::new(typed_arr), Box::new(typed_ind))
            }
            Expr::ArrayUpdate(arr, ind, new_val) => {
                let typed_arr = arr.typecheck(
                    struct_hm,
                    None,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input.clone(),
                );
                let elem_type = match extract_type(&typed_arr) {
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

                TypedExpr::ArrayUpdate(
                    elem_type,
                    Box::new(typed_arr),
                    Box::new(typed_ind),
                    Box::new(typed_new_val),
                )
            }
            Expr::ArrayAlloc(elem_t, len) => {
                validate_type(&elem_t, struct_hm).unwrap();

                let parsed_len = len.typecheck(
                    struct_hm,
                    Some(&ExprType::Int),
                    type_bindings,
                    function_sigs,
                    struct_sigs,
                    allow_input,
                );

                TypedExpr::ArrayAlloc(elem_t, Box::new(parsed_len))
            }
            Expr::ArrayLen(arr) => {
                let typed_arr = arr.typecheck(
                    struct_hm,
                    None,
                    type_bindings,
                    function_sigs,
                    struct_sigs,
                    allow_input,
                );
                match extract_type(&typed_arr) {
                    ExprType::Array(_) => (),
                    _ => panic!("arr_len called on expression which isn't an array"),
                }

                TypedExpr::ArrayLen(ExprType::Int, Box::new(typed_arr))
            }
        };

        if let Some(expected_type) = expected_type {
            let got_type = extract_type(&tchecked_exr);
            if got_type != *expected_type {
                panic!("Invalid: Type mismatch; expected expression {:?} to have type {:?} but it has type {:?}", tchecked_exr, expected_type, got_type)
            }
        }

        tchecked_exr
    }
}

impl UserFunction {
    fn typecheck(
        self,
        struct_sigs: im::HashMap<String, StructSignature>,
        function_sigs: im::HashMap<String, FunSignature>,
        struct_hm: &HashMap<String, i32>,
    ) -> TypedFunction {
        let UserFunction::UserFun(name, FunSignature::Sig(ret_type, param_types), body) = self;

        // insert args into type bindings
        let mut type_bindings: HashMap<String, ExprType> = im::HashMap::new();
        for (param_type, param_name) in param_types.iter() {
            match type_bindings.get(param_name) {
                Some(_) => panic!("Duplicate Argument"),
                None => type_bindings.insert(param_name.to_string(), param_type.clone()), // TODO CLONE
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

        TypedFunction::Fun(
            name,
            FunSignature::Sig(ret_type, param_types),
            type_checked_body,
        )
    }
}

impl Prog {
    pub fn typecheck(self) -> TypedProg {
        let Prog::Program(structs, functions, body) = self;

        /* --- Typecheck Structs --- */

        // read all structs into a map of struct name => type enum
        let mut struct_enum_map: HashMap<String, i32> = HashMap::new();
        for UserStruct::UserStruct(struct_name, _) in structs.iter() {
            // get a type enumeration for our struct name, creating it if it doens't exist
            let our_type_enum = struct_name_to_type_enum(&struct_name);

            // check if the struct was already declared
            match struct_enum_map.get(struct_name) {
                Some(_) => panic!("Invalid: duplicate struct declaration: {}", struct_name),
                None => {
                    // map struct name => type enum
                    // unlike the more general maps in semantics.rs, this maps only declared structs
                    struct_enum_map.insert(struct_name.to_string(), our_type_enum);
                    // TODO CLONE
                }
            }
        } // struct_enum_map maps declared struct names => type enumeration

        // typecheck each struct (ie, all fields valid), building a map of {struct type enumeration => StructType }
        let mut struct_type_map: im::HashMap<String, StructSignature> = im::HashMap::new();
        let mut struct_layouts: HashMap<String, StructLayout> = im::HashMap::new();
        for UserStruct::UserStruct(struct_name, StructSignature::Sig(field_names)) in structs {
            let mut checked_struct_fields: Vec<(ExprType, String)> = Vec::new();
            let mut struct_fields_names: HashMap<String, i32> = im::HashMap::new();
            let mut i = 0;
            for (field_type, field_name) in field_names {
                if let Some(_) = struct_fields_names.insert(field_name.to_string(), i) {
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
                checked_struct_fields.push((checked_field_type, field_name.to_string()));
            }

            let struct_sig = StructSignature::Sig(checked_struct_fields);
            let struct_layout = StructLayout::Layout(struct_fields_names);

            // push this sig into the struct type map
            struct_type_map.insert(struct_name.to_string(), struct_sig);
            struct_layouts.insert(struct_name.to_string(), struct_layout);
        }

        /* --- Typecheck Functions --- */

        // read all functions into map of function name to type, checking for dupes and illegal names
        let mut function_sigs: im::HashMap<String, FunSignature> = im::HashMap::new();
        for UserFunction::UserFun(name, function_sig, _) in functions.iter() {
            match function_sigs.get(name) {
                Some(_) => panic!("Duplicate Function Definition"),
                None => {
                    function_sigs.insert(name.to_string(), function_sig.to_owned()); // TODO CLONE

                    let FunSignature::Sig(expr_type, vec) = &function_sig;
                    validate_type(expr_type, &struct_enum_map).unwrap();
                    for (v, _) in vec {
                        validate_type(v, &struct_enum_map).unwrap();
                    }
                }
            };
        }

        let typed_functions = functions
            .into_iter()
            .map(|f| f.typecheck( struct_type_map.clone(), function_sigs.clone(), &struct_enum_map))
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
        TypedProg::Program(
            extract_type(&typed_body),
            struct_type_map,
            struct_layouts,
            typed_functions,
            typed_body,
        )
    }
}
