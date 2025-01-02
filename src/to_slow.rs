/* --- OOP Recursive to slow method--- */

use crate::{structs::*, typecheck_fast_shared::TypeCheckState};

impl<'a> FastTypedProg<'a> {
    pub fn to_slow(&self) -> TypedProg {
        let FastTypedProg::Program(expr_type, type_map, hash_map, hash_map1, vec, fast_typed_expr) =
            self;
        let new_map = im::HashMap::from_iter(
            hash_map
                .into_iter()
                .map(|(key, val)| (key.to_string(), val.to_slow())),
        );

        let new_map1 = im::HashMap::from_iter(
            hash_map1
                .into_iter()
                .map(|(key, val)| (key.to_string(), val.to_slow())),
        );
        TypedProg::Program(
            expr_type.to_expr_type(&type_map),
            new_map,
            new_map1,
            vec.into_iter().map(|fte| fte.to_slow(&type_map)).collect(),
            fast_typed_expr.to_slow(&type_map),
        )
    }
}

impl<'a> FastStructLayout<'a> {
    pub fn to_slow(&self) -> StructLayout {
        let FastStructLayout::Layout(hash_map) = self;
        StructLayout::Layout(im::HashMap::from_iter(
            hash_map
                .into_iter()
                .map(|(name_str, offset)| (name_str.to_string(), *offset)),
        ))
    }
}

impl<'a> FastStructSignature<'a> {
    pub fn to_slow(&self) -> StructSignature {
        let FastStructSignature::Sig(vec) = self;
        StructSignature::Sig(
            vec.into_iter()
                .map(|(field_type, field_str)| (field_type.clone(), field_str.to_string()))
                .collect(),
        )
    }
}

impl<'a> FastTypedFunction<'a> {
    pub fn to_slow(&self, type_set: &TypeCheckState) -> TypedFunction {
        let FastTypedFunction::Fun(name_str, fast_fun_signature, fast_typed_expr) = self;
        TypedFunction::Fun(
            name_str.to_string(),
            fast_fun_signature.to_slow(),
            fast_typed_expr.to_slow(type_set),
        )
    }
}

impl<'a> FastFunSignature<'a> {
    fn to_slow(&self) -> FunSignature {
        let FastFunSignature::Sig(expr_type, vec) = self;
        FunSignature::Sig(
            expr_type.to_owned(),
            vec.into_iter()
                .map(|(expr_type, str)| (expr_type.clone(), str.to_string()))
                .collect(),
        )
    }
}

impl FastExprType {
    fn to_slow(&self, type_set: &TypeCheckState) -> ExprType {
        self.to_expr_type(type_set)
    }
}

impl<'a> FastTypedExpr<'a> {
    pub fn to_slow(&self, type_set: &TypeCheckState) -> TypedExpr {
        match self {
            FastTypedExpr::__INTERNAL => unreachable!(),
            FastTypedExpr::Number(x) => TypedExpr::Number(*x),
            FastTypedExpr::Boolean(b) => TypedExpr::Boolean(*b),
            FastTypedExpr::Id(expr_type, name) => {
                TypedExpr::Id(expr_type.to_slow(type_set), name.to_string())
            }
            FastTypedExpr::Let(expr_type, vec, fast_typed_expr) => TypedExpr::Let(
                expr_type.to_slow(type_set),
                vec.into_iter()
                    .map(|(s, fte)| (s.to_string(), fte.to_slow(type_set)))
                    .collect(),
                Box::new(fast_typed_expr.to_slow(type_set)),
            ),
            FastTypedExpr::UnOp(expr_type, op1, fast_typed_expr) => TypedExpr::UnOp(
                expr_type.to_slow(type_set),
                *op1,
                Box::new(fast_typed_expr.to_slow(type_set)),
            ),
            FastTypedExpr::BinOp(expr_type, op2, fast_typed_expr, fast_typed_expr1) => {
                TypedExpr::BinOp(
                    expr_type.to_slow(type_set),
                    *op2,
                    Box::new(fast_typed_expr.to_slow(type_set)),
                    Box::new(fast_typed_expr1.to_slow(type_set)),
                )
            }
            FastTypedExpr::If(expr_type, fast_typed_expr, fast_typed_expr1, fast_typed_expr2) => {
                TypedExpr::If(
                    expr_type.to_slow(type_set),
                    Box::new(fast_typed_expr.to_slow(type_set)),
                    Box::new(fast_typed_expr1.to_slow(type_set)),
                    Box::new(fast_typed_expr2.to_slow(type_set)),
                )
            }
            FastTypedExpr::RepeatUntil(expr_type, fast_typed_expr, fast_typed_expr1) => {
                TypedExpr::RepeatUntil(
                    expr_type.to_slow(type_set),
                    Box::new(fast_typed_expr.to_slow(type_set)),
                    Box::new(fast_typed_expr1.to_slow(type_set)),
                )
            }
            FastTypedExpr::Set(expr_type, s, fast_typed_expr) => TypedExpr::Set(
                expr_type.to_slow(type_set),
                s.to_string(),
                Box::new(fast_typed_expr.to_slow(type_set)),
            ),
            FastTypedExpr::Block(expr_type, vec) => TypedExpr::Block(
                expr_type.to_slow(type_set),
                vec.into_iter().map(|fte| fte.to_slow(type_set)).collect(),
            ),
            FastTypedExpr::FunName(expr_type, name) => {
                TypedExpr::FunName(expr_type.to_slow(type_set), name.to_string())
            }
            FastTypedExpr::Call(expr_type, fast_typed_expr, vec) => TypedExpr::Call(
                expr_type.to_slow(type_set),
                Box::new(fast_typed_expr.to_slow(type_set)),
                vec.into_iter().map(|fte| fte.to_slow(type_set)).collect(),
            ),
            FastTypedExpr::Input => TypedExpr::Input,
            FastTypedExpr::RDInput => TypedExpr::RDInput,
            FastTypedExpr::Null(expr_type) => TypedExpr::Null(expr_type.to_slow(type_set)),
            FastTypedExpr::Alloc(expr_type) => TypedExpr::Alloc(expr_type.to_slow(type_set)),
            FastTypedExpr::Update(expr_type, fast_typed_expr, name, fast_typed_expr1) => {
                TypedExpr::Update(
                    expr_type.to_slow(type_set),
                    Box::new(fast_typed_expr.to_slow(type_set)),
                    name.to_string(),
                    Box::new(fast_typed_expr1.to_slow(type_set)),
                )
            }
            FastTypedExpr::Lookup(expr_type, fast_typed_expr, name) => TypedExpr::Lookup(
                expr_type.to_slow(type_set),
                Box::new(fast_typed_expr.to_slow(type_set)),
                name.to_string(),
            ),
            FastTypedExpr::Unit => TypedExpr::Unit,
            FastTypedExpr::ArrayAlloc(expr_type, fast_typed_expr) => {
                match expr_type.to_slow(type_set) {
                    ExprType::Array(elem_type) => TypedExpr::ArrayAlloc(
                        *elem_type,
                        Box::new(fast_typed_expr.to_slow(type_set)),
                    ),
                    _ => panic!("array alloc for fast typed expressions should have array type"),
                }
            }
            FastTypedExpr::ArrayLookup(expr_type, fast_typed_expr, fast_typed_expr1) => {
                TypedExpr::ArrayLookup(
                    expr_type.to_slow(type_set),
                    Box::new(fast_typed_expr.to_slow(type_set)),
                    Box::new(fast_typed_expr1.to_slow(type_set)),
                )
            }
            FastTypedExpr::ArrayUpdate(
                expr_type,
                fast_typed_expr,
                fast_typed_expr1,
                fast_typed_expr2,
            ) => TypedExpr::ArrayUpdate(
                expr_type.to_slow(type_set),
                Box::new(fast_typed_expr.to_slow(type_set)),
                Box::new(fast_typed_expr1.to_slow(type_set)),
                Box::new(fast_typed_expr2.to_slow(type_set)),
            ),
            FastTypedExpr::ArrayLen(expr_type, fast_typed_expr) => TypedExpr::ArrayLen(
                expr_type.to_slow(type_set),
                Box::new(fast_typed_expr.to_slow(type_set)),
            ),
        }
    }
}
