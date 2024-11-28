use crate::structs::*;

fn optimize_expr(e: &TypedExpr) -> TypedExpr {
    match e {
        TypedExpr::Number(n) => TypedExpr::Number(*n),
        TypedExpr::Boolean(b) => TypedExpr::Boolean(*b),
        TypedExpr::Id(t, s) => TypedExpr::Id(*t, s.clone()),
        TypedExpr::Let(t, assignations, e) => {
            let optimized_e = Box::new(optimize_expr(e));
            let mut optimized_assignations = Vec::new();

            for (s, expr) in assignations {
                optimized_assignations.push((s.clone(), optimize_expr(expr)));
            };

            TypedExpr::Let(*t, optimized_assignations, optimized_e)
        },
        TypedExpr::UnOp(t, op, e) => {
            let optimized_e = Box::new(optimize_expr(e));
            
            match op {
                Op1::Add1 => {
                    match *optimized_e {
                        TypedExpr::Number(n) => match n.checked_add(1) {
                            Some(v) => TypedExpr::Number(v),
                            _ => panic!("overflow"),
                        },
                        _ => TypedExpr::UnOp(*t, *op, optimized_e),
                    }
                },
                Op1::Sub1 => {
                    match *optimized_e {
                        TypedExpr::Number(n) => match n.checked_sub(1) {
                            Some(v) => TypedExpr::Number(v),
                            _ => panic!("overflow"),
                        },
                        _ => TypedExpr::UnOp(*t, *op, optimized_e),
                    }
                },
                Op1::Print => TypedExpr::UnOp(*t, *op, optimized_e),
                Op1::Not => {
                    match *optimized_e {
                        TypedExpr::Boolean(b) => TypedExpr::Boolean(!b),
                        _ => TypedExpr::UnOp(*t, *op, optimized_e),
                    }
                },
            } 
        },
        TypedExpr::BinOp(t, op, l, r) => {
            // Optimize
            let optimized_l = Box::new(optimize_expr(l));
            let optimized_r = Box::new(optimize_expr(r));
            
            match op {
                Op2::Plus => match *optimized_l {
                    TypedExpr::Number(n1) => match *optimized_r {
                        TypedExpr::Number(n2) => match n1.checked_add(n2) {
                            Some(v) => TypedExpr::Number(v),
                            _ => panic!("overflow"),
                        },
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                },
                Op2::Minus => match *optimized_l {
                    TypedExpr::Number(n1) => match *optimized_r {
                        TypedExpr::Number(n2) => match n1.checked_sub(n2) {
                            Some(v) => TypedExpr::Number(v),
                            _ => panic!("overflow"),
                        },
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                },
                Op2::Times => match *optimized_l {
                    TypedExpr::Number(n1) => match *optimized_r {
                        TypedExpr::Number(n2) => match n1.checked_mul(n2) {
                            Some(v) => TypedExpr::Number(v),
                            _ => panic!("overflow"),
                        },
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                },
                Op2::Or => match *optimized_l {
                    TypedExpr::Boolean(b1) => match *optimized_r {
                        TypedExpr::Boolean(b2) => TypedExpr::Boolean(b1 || b2),
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                },
                Op2::And => match *optimized_l {
                    TypedExpr::Boolean(b1) => match *optimized_r {
                        TypedExpr::Boolean(b2) => TypedExpr::Boolean(b1 && b2),
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                },
                Op2::Greater => match *optimized_l {
                    TypedExpr::Number(n1) => match *optimized_r {
                        TypedExpr::Number(n2) => TypedExpr::Boolean(n1 > n2),
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                },
               Op2::GreaterEqual => match *optimized_l {
                    TypedExpr::Number(n1) => match *optimized_r {
                        TypedExpr::Number(n2) => TypedExpr::Boolean(n1 >= n2),
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                },
               Op2::Less => match *optimized_l {
                    TypedExpr::Number(n1) => match *optimized_r {
                        TypedExpr::Number(n2) => TypedExpr::Boolean(n1 < n2),
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                },
               Op2::LessEqual => match *optimized_l {
                    TypedExpr::Number(n1) => match *optimized_r {
                        TypedExpr::Number(n2) => TypedExpr::Boolean(n1 <= n2),
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                },
                Op2::Equal => match *optimized_l {
                    TypedExpr::Number(n1) => match *optimized_r {
                        TypedExpr::Number(n2) => TypedExpr::Boolean(n1 == n2),
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    TypedExpr::Boolean(b1) => match *optimized_r {
                        TypedExpr::Boolean(b2) => TypedExpr::Boolean(b1 == b2),
                        _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                    },
                    _ => TypedExpr::BinOp(*t, *op, optimized_l, optimized_r),
                },
            }
        },
        TypedExpr::If(t, cond, if_true, if_false) => {
            let optimized_cond = Box::new(optimize_expr(cond));
            let optimized_true = Box::new(optimize_expr(if_true));
            let optimized_false = Box::new(optimize_expr(if_false));
            
            match *optimized_cond {
                TypedExpr::Boolean(b) => {
                    if b {
                        *optimized_true
                    } else {
                        *optimized_false
                    }
                },
                _ => TypedExpr::If(*t, optimized_cond, optimized_true, optimized_false)
            }
        },
        TypedExpr::RepeatUntil(t, body, cond) => {
            let optimized_body = Box::new(optimize_expr(body));
            let optimized_cond = Box::new(optimize_expr(cond));
            
            TypedExpr::RepeatUntil(*t, optimized_body, optimized_cond)
        },
        TypedExpr::Set(t, var, e) => {
            let optimized_e = Box::new(optimize_expr(e));
            
            TypedExpr::Set(*t, var.clone(), optimized_e)
        },
        TypedExpr::Block(t, lines) => { 
            let mut optimized_lines = Vec::new();
            
            for line in lines {
                optimized_lines.push(optimize_expr(line));
            };

            TypedExpr::Block(*t, optimized_lines)
        },
        TypedExpr::Call(t, s, args) => {
            let mut optimized_args = Vec::new();
            
            for arg in args {
                optimized_args.push(optimize_expr(arg));
            };

            TypedExpr::Call(*t, s.clone(), optimized_args) 
        },
        TypedExpr::Input => TypedExpr::Input,
        TypedExpr::RDInput => TypedExpr::RDInput,
        TypedExpr::Null(t) => TypedExpr::Null(*t),
        TypedExpr::Alloc(t) => TypedExpr::Alloc(*t),
        TypedExpr::Update(t, var, field, e) => {
            let optimized_e = Box::new(optimize_expr(e));
            
            TypedExpr::Update(*t, var.clone(), field.clone(), optimized_e)
        },
        TypedExpr::Lookup(t, var, field) => TypedExpr::Lookup(*t, var.clone(), field.clone()),
        
        TypedExpr::Unit => TypedExpr::Unit,
    }
}

fn optimize_fn(f: &TypedFunction) -> TypedFunction {
    let TypedFunction::Fun(fun_name, FunSignature::Sig(ret_type, args), typed_body) = f;

    let optimized_body = optimize_expr(typed_body);

    TypedFunction::Fun(fun_name.clone(), FunSignature::Sig(*ret_type, args.clone()), optimized_body)
}


pub fn optimize_prog(tp: &TypedProg) -> TypedProg {
    let TypedProg::Program(body_type, struct_sigs, struct_layouts, typed_funs, typed_e) = tp;
    
    let mut optimized_fns = Vec::new();

    for typed_fun in typed_funs {
        optimized_fns.push(optimize_fn(typed_fun));
    };

    let optimized_e = optimize_expr(typed_e);

    TypedProg::Program(*body_type, struct_sigs.clone(), struct_layouts.clone(), optimized_fns, optimized_e)
}

