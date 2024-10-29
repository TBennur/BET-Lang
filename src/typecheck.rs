use im::HashMap;

fn struct_sig_type_of(struct_sig: &StructSignature, field_name: &String) -> Option<ExprType> {
    let StructSignature::Sig(field_names) = struct_sig;
    for (field_type, name) in field_names {
        if name == field_name {
            return Some(*field_type);
        }
    }
    None
}

fn can_coerce(from: ExprType, to: ExprType) -> bool {
    return from == to
        || match to {
            ExprType::StructPointer(_) => from == ExprType::Null,
            _ => false,
        };
}

use crate::{
    semantics::{struct_name_to_type_enum, struct_type_enum_to_name, STRUCT_NAME_TO_NUM},
    structs::*,
};

pub fn type_check_prog(p: &Prog) -> TypedProg {
    let Prog::Program(structs, functions, body) = p;

    /* --- Typecheck Structs --- */

    // read all structs into a map of struct name => type enum
    let mut struct_enum_map: HashMap<String, i32> = HashMap::new();
    for UserStruct::UserStruct(struct_name, _) in structs {
        // get a type enumeration for our struct name, creating it if it doens't exist
        let our_type_enum = struct_name_to_type_enum(&struct_name);

        // check if the struct was already declared
        match struct_enum_map.get(struct_name) {
            Some(_) => panic!("Invalid: duplicate struct declaration: {}", struct_name),
            None => {
                // map struct name => type enum
                // unlike the more general maps in semantics.rs, this maps only declared structs
                struct_enum_map.insert(struct_name.to_string(), our_type_enum);
            }
        }
    } // struct_enum_map maps declared struct names => type enumeration

    // typecheck each struct (ie, all fields valid), building a map of {struct type enumeration => StructType }
    let mut struct_type_map: HashMap<String, StructSignature> = im::HashMap::new();
    for UserStruct::UserStruct(struct_name, field_names) in structs {
        let checked_struct_fields = field_names.into_iter().map(
            | (field_type, field_name) |  {
                let checked_field_type = match field_type {
                    /* --- valid base types (struct fields can't be functions) --- */
                    ExprType::Int => ExprType::Int,
                    ExprType::Bool => ExprType::Bool,

                    /* --- a field with type null?? illegal --- */
                    ExprType::Null => panic!(
                        "Invalid: struct has field which expects type null ({}::{})",
                        struct_name, field_name
                    ),

                    /* --- field with type pointer to struct... check that the struct it points to exists! --- */
                    ExprType::StructPointer(pointed_struct_enum) => {
                        match struct_type_enum_to_name(*pointed_struct_enum) {
                            None => panic!("Invalid: struct {} has field {} which is a pointer to a non-declared struct type!", struct_name, field_name),
                            Some(pointed_struct_name) => match struct_enum_map.get(&pointed_struct_name) {
                                None => panic!("Invalid: struct {} has field {} which is a pointer to a non-declared struct type!", struct_name, field_name),
                                Some(&lookup_res) => {
                                    if lookup_res != *pointed_struct_enum {
                                        panic!("Invalid: struct {} has field {} which is a pointer to a non-declared struct type!", struct_name, field_name);
                                    }
                                    ExprType::StructPointer(lookup_res)
                                },
                            }
                        }
                    }
                };
            (checked_field_type, field_name.to_string())
            }
        ).collect();

        let struct_sig = StructSignature::Sig(checked_struct_fields);

        // push this sig into the struct type map
        struct_type_map.insert(struct_name.to_string(), struct_sig);
    }

    /* --- Typecheck Functions --- */

    // read all functions into map of function name to type, checking for dupes and illegal names
    let mut function_sigs: HashMap<String, FunSignature> = im::HashMap::new();
    for UserFunction::UserFun(name, function_sig, _) in functions {
        match function_sigs.get(name) {
            Some(_) => panic!("Duplicate Function Definition"),
            None => function_sigs.insert(name.to_string(), function_sig.to_owned()),
        };
    }

    let mut typed_functions = Vec::new();

    // typecheck each function
    for UserFunction::UserFun(name, FunSignature::Sig(ret_type, param_types), body) in functions {
        // insert args into type bindings
        let mut type_bindings: HashMap<String, ExprType> = im::HashMap::new();
        for (param_type, param_name) in param_types {
            match type_bindings.get(param_name) {
                Some(_) => panic!("Duplicate Argument"),
                None => type_bindings.insert(param_name.to_string(), *param_type),
            };
        }

        // get actual return type of body
        let type_checked_body = type_check_expr(
            body,
            type_bindings,
            function_sigs.clone(),
            struct_type_map.clone(),
            false,
        );

        // compare to signature type
        if *ret_type != extract_type(&type_checked_body) {
            panic!("mismatched function signature & body");
        }

        let typed_function = TypedFunction::Fun(
            name.to_string(),
            FunSignature::Sig(extract_type(&type_checked_body), param_types.to_vec()),
            type_checked_body,
        );

        typed_functions.push(typed_function);
    }

    /* --- Typecheck Program Body --- */

    // allow input in the body of the program
    let typed_body = type_check_expr(
        body,
        im::HashMap::new(),
        function_sigs.clone(),
        struct_type_map.clone(),
        true,
    );
    TypedProg::Program(extract_type(&typed_body), typed_functions, typed_body)
}

fn type_check_expr(
    e: &Expr,
    type_bindings: im::HashMap<String, ExprType>,
    function_sigs: im::HashMap<String, FunSignature>,
    struct_sigs: im::HashMap<String, StructSignature>,
    allow_input: bool,
) -> TypedExpr {
    match e {
        Expr::Input => {
            if !allow_input {
                panic!("Invalid: Input is not an Int")
            }

            TypedExpr::Input
        }

        Expr::Boolean(b) => TypedExpr::Boolean(*b),

        Expr::Id(id) => match type_bindings.get(id) {
            None => panic!("Invalid: Unbound variable identifier {}", id),
            Some(t) => TypedExpr::Id(*t, id.clone()),
        },
        Expr::Number(n) => TypedExpr::Number(*n),

        Expr::UnOp(op1, expr) => {
            let typed_expr = type_check_expr(
                expr,
                type_bindings,
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            match op1 {
                Op1::Add1 | Op1::Sub1 => match extract_type(&typed_expr) {
                    ExprType::Int => TypedExpr::UnOp(ExprType::Int, *op1, Box::new(typed_expr)),
                    _ => panic!("Type mismatch in UnOp"),
                },
                Op1::Print => {
                    TypedExpr::UnOp(extract_type(&typed_expr), *op1, Box::new(typed_expr))
                }
            }
        }

        Expr::Set(name, new_value) => {
            // fails if the name isn't in scope
            let t1 = *match type_bindings.get(name) {
                Some(t1) => t1,
                None => panic!("Invalid: Unbound variable identifier {}", name),
            };

            // can only "set" a variable to the same type within the current scope
            let t1_prime = type_check_expr(
                new_value,
                type_bindings.clone(),
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            if extract_type(&t1_prime) != t1 {
                panic!("Type mismatch in Set")
            }
            TypedExpr::Set(extract_type(&t1_prime), name.clone(), Box::new(t1_prime))
        }

        Expr::Let(bindings, finally) => {
            let mut curr_let_binding = type_bindings;
            let mut in_this_let: im::HashSet<String> = im::HashSet::new();
            let mut bindings_typed_exnr: Vec<(String, TypedExpr)> = Vec::new();

            for (id, exp) in bindings {
                // check for duplicates
                match in_this_let.insert(id.to_string()) {
                    None => (),
                    Some(_) => panic!("Duplicate binding"),
                };

                // typecheck the expression
                // panics if if doesn't typecheck
                let typed_expr = type_check_expr(
                    exp,
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
            let final_typed_expr = type_check_expr(
                finally,
                curr_let_binding,
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            TypedExpr::Let(
                extract_type(&final_typed_expr),
                bindings_typed_exnr,
                Box::new(final_typed_expr),
            )
        }

        Expr::BinOp(op2, a, b) => match op2 {
            // int * int => int
            Op2::Plus | Op2::Minus | Op2::Times => {
                let a_typed_exprn = type_check_expr(
                    a,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                if extract_type(&a_typed_exprn) != ExprType::Int {
                    panic!("Type mismatch: BinOp argument not an Int");
                }

                let b_typed_exprn = type_check_expr(
                    b,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                if extract_type(&b_typed_exprn) != ExprType::Int {
                    panic!("Type mismatch: BinOp argument not an Int");
                }

                TypedExpr::BinOp(
                    ExprType::Int,
                    *op2,
                    Box::new(a_typed_exprn),
                    Box::new(b_typed_exprn),
                )
            }

            // t * t => bool
            Op2::Equal => {
                let a_typed_exprn = type_check_expr(
                    a,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                let b_typed_exprn = type_check_expr(
                    b,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                if extract_type(&a_typed_exprn) != extract_type(&b_typed_exprn) {
                    panic!("Type mismatch: Equal has different argument types");
                }

                TypedExpr::BinOp(
                    ExprType::Bool,
                    *op2,
                    Box::new(a_typed_exprn),
                    Box::new(b_typed_exprn),
                )
            }

            // int * int => bool
            Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                let a_typed_exprn = type_check_expr(
                    a,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                if extract_type(&a_typed_exprn) != ExprType::Int {
                    panic!("Type mismatch: BinOp argument not an Int");
                }

                let b_typed_exprn = type_check_expr(
                    b,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                if extract_type(&b_typed_exprn) != ExprType::Int {
                    panic!("Type mismatch: BinOp argument not an Int");
                }

                TypedExpr::BinOp(
                    ExprType::Bool,
                    *op2,
                    Box::new(a_typed_exprn),
                    Box::new(b_typed_exprn),
                )
            }
        },

        // bool * t2 * t2 => t2
        Expr::If(cond, val_if_true, val_if_false) => {
            // cond should typecheck to bool
            let typed_cond = type_check_expr(
                cond,
                type_bindings.clone(),
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            if extract_type(&typed_cond) != ExprType::Bool {
                panic!("Type mismatch: If condition not an Bool");
            };

            let typed_if_true = type_check_expr(
                val_if_true,
                type_bindings.clone(),
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            let typed_if_false = type_check_expr(
                val_if_false,
                type_bindings.clone(),
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            if extract_type(&typed_if_true) == extract_type(&typed_if_false) {
                TypedExpr::If(
                    extract_type(&typed_if_true),
                    Box::new(typed_cond),
                    Box::new(typed_if_true),
                    Box::new(typed_if_false),
                )
            } else {
                panic!("Type mismatch: If clauses have different types")
            }
        }

        // t1 * t2 => t1
        Expr::RepeatUntil(body, stop_cond) => {
            // stop_cond must be a bool
            let typed_stop_cond = type_check_expr(
                stop_cond,
                type_bindings.clone(),
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            if extract_type(&typed_stop_cond) != ExprType::Bool {
                panic!("Type mismatch: RepeatUntil condition not an Bool")
            }

            // repeat-until evaluates to the body (once stop_cond is true)
            let typed_body = type_check_expr(
                body,
                type_bindings.clone(),
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            TypedExpr::RepeatUntil(
                extract_type(&typed_body),
                Box::new(typed_body.clone()),
                Box::new(typed_stop_cond.clone()),
            )
        }

        Expr::Block(expns) => {
            if expns.is_empty() {
                panic!("Invalid: Block is Empty")
            }
            let mut block_typed_exprn: Vec<TypedExpr> = Vec::new();

            let mut final_type = ExprType::Int; // arbitrary
            for expr in expns {
                // typecheck each expression in the block
                let typed_exprn = type_check_expr(
                    expr,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );
                final_type = extract_type(&typed_exprn);
                block_typed_exprn.push(typed_exprn.clone());
            }
            // since block evaluates to the type of the last expression, that's the type of the block

            TypedExpr::Block(final_type, block_typed_exprn)
        }
        Expr::Call(fun_name, arguments) => {
            // check that function exists
            let fun_sig = match function_sigs.get(fun_name) {
                Some(fun_sig) => fun_sig,
                None => panic!(
                    "Invalid: Called function {:?}, which doesn't exist",
                    fun_name
                ),
            };

            let FunSignature::Sig(return_type, param_types) = fun_sig;

            // check that there's the correct number of arguments
            if param_types.len() != arguments.len() {
                panic!("Invalid: Called function with wrong number of arguments")
            }

            // check that arguments are well typed, and agree with function sig
            let zipped: Vec<(&Expr, &(ExprType, String))> =
                arguments.iter().zip(param_types.iter()).collect();

            let mut typed_args = Vec::new();
            for (arg_exp, (expr_type, _)) in zipped {
                let arg_typed = type_check_expr(
                    arg_exp,
                    type_bindings.clone(),
                    function_sigs.clone(),
                    struct_sigs.clone(),
                    allow_input,
                );

                if extract_type(&arg_typed) != *expr_type {
                    panic!("Called function with mismatched arg types")
                }

                typed_args.push(arg_typed);
            }

            // Check function
            TypedExpr::Call(*return_type, fun_name.to_string(), typed_args)
        }
        Expr::Null => TypedExpr::Null,
        Expr::Alloc(struct_type) => {
            let name_to_num_map = STRUCT_NAME_TO_NUM.lock().unwrap();
            match name_to_num_map.get(struct_type) {
                Some(&struct_type_int) => {
                    TypedExpr::Alloc(ExprType::StructPointer(struct_type_int))
                }
                None => panic!(
                    "Invalid: invalid allocate of unknown struct type: {}",
                    struct_type
                ),
            }
        }
        Expr::Update(pointer, field_name, new_value) => {
            // first expression must be a pointer type
            let pointer_typed_expr = type_check_expr(
                pointer,
                type_bindings.clone(),
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            let pointed_struct_enum = match extract_type(&pointer_typed_expr) {
                ExprType::StructPointer(struct_enum) => struct_enum,
                _ => panic!(
                    "Invalid: Update on non-pointer-typed expression: {:?}",
                    pointer
                ),
            };

            // the kind of struct it points to must have a corresponding field
            let pointed_struct_name = match struct_type_enum_to_name(pointed_struct_enum) {
                Some(s) => s,
                None => panic!(
                    "Update with non-existent struct with type enumeration: {}",
                    pointed_struct_enum
                ),
            };

            let struct_sig = match struct_sigs.get(&pointed_struct_name) {
                Some(sig) => sig,
                None => panic!("Update with non-existent struct: {}", pointed_struct_name),
            };

            let expected_type = match struct_sig_type_of(struct_sig, field_name) {
                Some(expr_type) => expr_type,
                None => panic!(""),
            };

            // get the type of the new value
            let update_typed_expr = type_check_expr(
                &new_value,
                type_bindings.clone(),
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            let update_type = extract_type(&update_typed_expr);

            if expected_type != update_type && !can_coerce(update_type, expected_type) {
                panic!(
                    "Invalid: Update type mismatch: expeced {:?} but got {:?}",
                    expected_type, update_type
                );
            }

            TypedExpr::Update(
                expected_type,
                Box::new(pointer_typed_expr),
                field_name.to_string(),
                Box::new(update_typed_expr),
            )
        }
        Expr::Lookup(pointer, field_name) => {
            // first expression must be a pointer type
            let pointer_typed_expr = type_check_expr(
                pointer,
                type_bindings.clone(),
                function_sigs.clone(),
                struct_sigs.clone(),
                allow_input,
            );
            let pointed_struct_enum = match extract_type(&pointer_typed_expr) {
                ExprType::StructPointer(struct_enum) => struct_enum,
                _ => panic!(
                    "Invalid: Update on non-pointer-typed expression: {:?}",
                    pointer
                ),
            };

            // the kind of struct it points to must have a corresponding field
            let pointed_struct_name = match struct_type_enum_to_name(pointed_struct_enum) {
                Some(s) => s,
                None => panic!(
                    "Update with non-existent struct with type enumeration: {}",
                    pointed_struct_enum
                ),
            };

            let struct_sig = match struct_sigs.get(&pointed_struct_name) {
                Some(sig) => sig,
                None => panic!("Update with non-existent struct: {}", pointed_struct_name),
            };

            let expected_type = match struct_sig_type_of(struct_sig, field_name) {
                Some(expr_type) => expr_type,
                None => panic!(""),
            };

            TypedExpr::Lookup(
                expected_type,
                Box::new(pointer_typed_expr),
                field_name.to_string(),
            )
        }
    }
}
