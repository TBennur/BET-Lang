use crate::structs::*;


pub fn type_check(e: &Expr, type_bindings: im::HashMap<String, ExprType>) -> TypedExpr {
    match e {
        Expr::Input => TypedExpr::Input(ExprType::Int),

        Expr::Boolean(b) => TypedExpr::Boolean(*b),

        Expr::Id(id) => match type_bindings.get(id) {
            None => panic!("Invalid"),
            Some(t) => TypedExpr::Id(*t, id.clone()),
        },
        Expr::Number(n) => TypedExpr::Number(*n),

        Expr::UnOp(op1, expr) => {
            let typed_expr = type_check(expr, type_bindings);
            match op1 {
                Op1::Add1 | Op1::Sub1 => match extract_type(&typed_expr) {
                    ExprType::Int => TypedExpr::UnOp(ExprType::Int, *op1, Box::new(typed_expr)),
                    _ => panic!("type mismatch"),
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
                None => panic!("Invalid"),
            };

            // can only "set" a variable to the same type within the current scope
            let t1_prime = type_check(new_value, type_bindings.clone());
            if extract_type(&t1_prime) != t1 {
                panic!("type mismatch")
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
                let typed_expr = type_check(exp, curr_let_binding.clone());

                // bind id to that type, allowing shadowing of different types
                curr_let_binding.insert(id.to_string(), extract_type(&typed_expr));

                // build binding part of the typed expression
                bindings_typed_exnr.push((id.to_string(), typed_expr));
            }

            // evaluate the type of the final expression after all the bindings
            let final_typed_expr = type_check(finally, curr_let_binding);
            TypedExpr::Let(
                extract_type(&final_typed_expr),
                bindings_typed_exnr,
                Box::new(final_typed_expr),
            )
        }

        Expr::BinOp(op2, a, b) => match op2 {
            // int * int => int
            Op2::Plus | Op2::Minus | Op2::Times => {
                let a_typed_exprn = type_check(a, type_bindings.clone());
                if extract_type(&a_typed_exprn) != ExprType::Int {
                    panic!("type mismatch");
                }

                let b_typed_exprn = type_check(b, type_bindings.clone());
                if extract_type(&b_typed_exprn) != ExprType::Int {
                    panic!("type mismatch");
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
                let a_typed_exprn = type_check(a, type_bindings.clone());
                let b_typed_exprn = type_check(b, type_bindings.clone());
                if extract_type(&a_typed_exprn) != extract_type(&b_typed_exprn) {
                    panic!("type mismatch");
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
                let a_typed_exprn = type_check(a, type_bindings.clone());
                if extract_type(&a_typed_exprn) != ExprType::Int {
                    panic!("type mismatch");
                }

                let b_typed_exprn = type_check(b, type_bindings.clone());
                if extract_type(&b_typed_exprn) != ExprType::Int {
                    panic!("type mismatch");
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
            let typed_cond = type_check(cond, type_bindings.clone());
            if extract_type(&typed_cond) != ExprType::Bool {
                panic!("type mismatch");
            };

            let typed_if_true = type_check(val_if_true, type_bindings.clone());
            let typed_if_false = type_check(val_if_false, type_bindings.clone());
            if extract_type(&typed_if_true) == extract_type(&typed_if_false) {
                TypedExpr::If(
                    extract_type(&typed_if_true),
                    Box::new(typed_cond),
                    Box::new(typed_if_true),
                    Box::new(typed_if_false),
                )
            } else {
                panic!("type mismatch")
            }
        }

        // t1 * t2 => t1
        Expr::RepeatUntil(body, stop_cond) => {
            // stop_cond must be a bool
            let typed_stop_cond = type_check(stop_cond, type_bindings.clone());
            if extract_type(&typed_stop_cond) != ExprType::Bool {
                panic!("type mismatch")
            }

            // repeat-until evaluates to the body (once stop_cond is true)
            let typed_body = type_check(body, type_bindings.clone());
            TypedExpr::RepeatUntil(
                extract_type(&typed_body),
                Box::new(typed_body.clone()),
                Box::new(typed_stop_cond.clone()),
            )
        }

        Expr::Block(expns) => {
            if expns.len() == 0 {
                panic!("invalid")
            }
            let mut block_typed_exprn: Vec<TypedExpr> = Vec::new();

            let mut final_type = ExprType::Int; // arbitrary
            for expr in expns {
                // typecheck each expression in the block
                let typed_exprn = type_check(expr, type_bindings.clone());
                final_type = extract_type(&typed_exprn);
                block_typed_exprn.push(typed_exprn.clone());
            }
            // since block evaluates to the type of the last expression, that's the type of the block

            TypedExpr::Block(final_type, block_typed_exprn)
        }
    }
}
