use crate::{semantics::struct_type_enum_to_name, structs::*};

fn expr_type_to_str(typ: ExprType) -> String {
    match typ {
        ExprType::Int => "int".to_string(),
        ExprType::Bool => "bool".to_string(),
        ExprType::StructPointer(type_enum) => struct_type_enum_to_name(type_enum).unwrap(),
    }
}

fn struct_to_bet(
    UserStruct::UserStruct(name, StructSignature::Sig(fields)): &UserStruct,
) -> String {
    let field_types_str = fields
        .into_iter()
        .map(|(field_type, field_name)| {
            format!("{}::{}", field_name, expr_type_to_str(*field_type))
        })
        .collect::<Vec<String>>()
        .join(", ");

    format!("struct {name}({field_types_str})")
}

fn fun_to_bet(f: &UserFunction) -> String {
    let UserFunction::UserFun(name, FunSignature::Sig(ret_type, param_types), body) = f;

    let params_str = param_types
        .into_iter()
        .map(|(param_type, param_name)| {
            format!("{}::{}", param_name, expr_type_to_str(*param_type))
        })
        .collect::<Vec<String>>()
        .join(", ");

    let body_str = expression_to_bet(body);
    let wrapped_body = match body {
        Expr::Block(_) => body_str,
        _ => wrap_as_block(body_str),
    };

    let ret_type_str = expr_type_to_str(*ret_type);

    format!("fun {name}({params_str})::{ret_type_str}{wrapped_body}")
}

pub fn prog_to_bet(prog: &Prog) -> String {
    let Prog::Program(structs, functions, body) = prog;
    let mut components: Vec<String> = Vec::new();
    for structure in structs {
        components.push(struct_to_bet(structure));
    }
    for function in functions {
        components.push(fun_to_bet(function));
    }
    let body_str = expression_to_bet(body);
    // let body_str = match *body {
    //     Expr::Block(_) => body_str,
    //     _ => wrap_as_block(body_str),
    // };
    components.push(body_str);
    components.join(";\n\n")
}

fn need_paren_wrap_for_val(expr: &Expr) -> bool {
    // whether you need to wrap an expression in parentheses to use it as a value
    match expr {
        // single
        Expr::Number(_) => false,
        Expr::Boolean(_) => false,
        Expr::Id(_) => false,
        Expr::Input => false,

        // already wrapped
        Expr::Block(_) => false,

        // composite
        Expr::Let(_, _) => true,
        Expr::UnOp(_, _) => true,
        Expr::BinOp(_, _, _) => true,
        Expr::If(_, _, _) => true,
        Expr::RepeatUntil(_, _) => true,
        Expr::Set(_, _) => true,
        Expr::Call(_, _) => true,
        Expr::Null(_) => true,
        Expr::Alloc(_) => true,
        Expr::Update(_, _, _) => true,
        Expr::Lookup(_, _) => true,
    }
}

fn prepend_to_each_line(s: &str, prefix: &str) -> String {
    s.lines()
        .map(|line| format!("{}{}", prefix, line))
        .collect::<Vec<String>>()
        .join("\n")
}

const INDENT: &str = "    ";

fn wrap_as_block(s: String) -> String {
    let indented = prepend_to_each_line(&s, INDENT);
    format!("{{\n{indented}\n}}")
}

fn expression_to_bet(expr: &Expr) -> String {
    match expr {
        Expr::Number(x) => {
            if *x >= 0 {
                x.to_string()
            }else {
                format!("({})", x.to_string())
            }
        },
        Expr::Boolean(b) => b.to_string(),
        Expr::Id(name) => name.to_string(),
        Expr::Let(bindings, body) => {
            let mut bindings_strs: Vec<String> = Vec::new();
            for (id, subexpr) in bindings {
                let non_wrapped_binding = expression_to_bet(subexpr);
                // don't need to wrap bindings in let as we scan for := from left to right
                bindings_strs.push(format!("{} := {}", id, non_wrapped_binding));
            }
            let bindings_str = bindings_strs.join(",\n");
            let indented_bindings = prepend_to_each_line(&bindings_str, INDENT);

            let body_str = expression_to_bet(&body);
            let wrapped_body_str = match **body {
                Expr::Block(_) => body_str, // don't wrap in block; already indented
                _ => wrap_as_block(body_str),
            };
            format!("let (\n{indented_bindings}\n){wrapped_body_str}")
        }

        Expr::UnOp(op1, expr) => {
            let expr_str = expression_to_bet(expr);
            // don't to wrap the arguments to unops, as unops will implictly place parens around everything to their left
            format!("{op1} {expr_str}")
        }

        Expr::BinOp(op2, lhs_expr, rhs_expr) => {
            // need to wrap for binops, as they rely on a struct lhs op2 rhs structure
            let mut lhs_str = expression_to_bet(lhs_expr);
            let mut rhs_str = expression_to_bet(rhs_expr);
            if need_paren_wrap_for_val(&lhs_expr) {
                lhs_str = format!("({lhs_str})");
            }
            if need_paren_wrap_for_val(&rhs_expr) {
                rhs_str = format!("({rhs_str})");
            }

            format!("{lhs_str} {op2} {rhs_str}")
        }
        Expr::If(cond, true_block, false_block) => {
            let cond_str = expression_to_bet(&cond);
            let true_block_str = expression_to_bet(&true_block);
            let wrapped_true_block_str = match **true_block {
                Expr::Block(_) => true_block_str, // don't wrap in block; already indented
                _ => wrap_as_block(true_block_str),
            };

            let false_block_str = expression_to_bet(&false_block);
            let wrapped_false_block_str = match **false_block {
                Expr::Block(_) => false_block_str, // don't wrap in block; already indented
                _ => wrap_as_block(false_block_str),
            };

            format!(
                "if ({}){}else{}",
                cond_str, wrapped_true_block_str, wrapped_false_block_str
            )
        }

        Expr::RepeatUntil(body, stop_cond) => {
            let stop_cond_str = expression_to_bet(&stop_cond);
            let body_str = expression_to_bet(body);
            let wrapped_body_str = match **body {
                Expr::Block(_) => body_str, // don't wrap in block; already indented
                _ => wrap_as_block(body_str),
            };
            format!("do {} until ({})", wrapped_body_str, stop_cond_str)
        }

        Expr::Set(id, new_val) => {
            let new_val_str = expression_to_bet(&new_val);
            // don't need to wrap with set, as we match on the id := <rest>
            format!("{id}:={new_val_str}")
        }

        Expr::Block(vec) => wrap_as_block(
            vec.into_iter()
                .map(expression_to_bet)
                .collect::<Vec<String>>()
                .join(";\n"),
        ),
        Expr::Input => "input".to_string(),
        Expr::Call(fn_name, args) => {
            let args_strs = args
                .into_iter()
                .map(expression_to_bet) // don't need to wrap any in parens as we have ',' delims
                .collect::<Vec<String>>()
                .join(",");
            format!("{fn_name}({args_strs})")
        }

        Expr::Null(ptr_type) => format!("null {ptr_type}"),
        Expr::Alloc(ptr_type) => format!("new {ptr_type}"),
        Expr::Update(ptr, field_name, new_val) => {
            // we parse from right to left when looking for ().field_name := (), so we need the rightmost to be wrapped, if needed
            let ptr_str = expression_to_bet(&ptr);
            let mut new_val_str = expression_to_bet(&new_val);
            if need_paren_wrap_for_val(&new_val) {
                new_val_str = format!("({new_val_str})");
            }

            format!("{ptr_str}.{field_name}:={new_val_str}")
        }
        Expr::Lookup(ptr, field_name) => {
            let ptr_str = expression_to_bet(ptr);
            format!("{ptr_str}.{field_name}") // don't need to wrap as ".field_name" is matched from right to left
        }
    }
}
