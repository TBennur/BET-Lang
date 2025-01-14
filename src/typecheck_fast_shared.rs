use crate::{
    compile::StructSerializer,
    semantics::struct_type_enum_to_name,
    structs::{
        ExprType, FastExpr, FastExprType, FastFunSignature, FastStructSignature, FastTypedExpr,
        FastTypedProg,
    },
};

/// Given a struct signature, lookup the type associated with the field with name field_name; or None if no such field exists
pub fn struct_sig_type_of<'a>(
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

pub fn validate_type_fast<'a>(
    expr_type: &FastExprType,
    struct_enum_map: &im::HashMap<&'a str, i32>,
    type_set: &TypeCheckState,
) -> Result<(), String> {
    validate_type(expr_type.to_expr_type_ref(type_set), struct_enum_map)
}

pub fn validate_type<'a>(
    expr_type: &ExprType,
    struct_enum_map: &im::HashMap<&'a str, i32>,
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

#[derive(Clone, Copy, PartialEq)]
pub enum ExpectedType {
    Any,
    Specific(FastExprType),
}

#[derive(Debug, Clone)]
pub struct TypeCheckState {
    counter: u32,
    num_to_type_expr: std::collections::HashMap<u32, ExprType>,
    type_expr_to_num: std::collections::HashMap<ExprType, u32>,
}

impl TypeCheckState {
    // ingest an owned expr type. bool is whether it was already in mapping
    pub fn ingest(&mut self, expr_type: ExprType) -> (FastExprType, bool) {
        // base type
        if let Some(res) = expr_type.is_base() {
            return (res, true);
        }

        // recursive types go in map
        match self.type_expr_to_num.get(&expr_type) {
            Some(&our_counter) => (FastExprType::Other(our_counter.try_into().unwrap()), true),
            None => {
                let our_counter = self.counter;
                self.counter += 1;
                self.type_expr_to_num.insert(expr_type.clone(), our_counter);
                self.num_to_type_expr.insert(our_counter, expr_type);
                (FastExprType::Other(our_counter.try_into().unwrap()), false)
            }
        }
    }

    pub fn to_fexpr_t_opt(&self, expr_type: &ExprType) -> Option<FastExprType> {
        // base type
        if let Some(res) = expr_type.is_base() {
            return Some(res);
        }

        // recursive types
        self.type_expr_to_num
            .get(expr_type)
            .and_then(|arg0| Some(FastExprType::Other(*arg0)))
    }

    pub fn to_expr_type_ref(&self, fast_type: FastExprType) -> Option<&ExprType> {
        match fast_type {
            FastExprType::Int => Some(&ExprType::Int),
            FastExprType::Unit => Some(&ExprType::Unit),
            FastExprType::Bool => Some(&ExprType::Bool),
            FastExprType::Other(type_enum) => self.num_to_type_expr.get(&type_enum),
        }
    }

    pub fn new() -> TypeCheckState {
        TypeCheckState {
            counter: 0,
            num_to_type_expr: std::collections::HashMap::new(),
            type_expr_to_num: std::collections::HashMap::new(),
        }
    }

    pub fn ingest_clone(&mut self, expr_type: &ExprType) -> FastExprType {
        match self.to_fexpr_t_opt(expr_type) {
            Some(t) => t,
            None => self.ingest(expr_type.clone()).0, // TODO CLONE
        }
    }
}

impl<'a> FastTypedExpr<'a> {
    pub fn check_type(self, expect: ExpectedType) -> FastTypedExpr<'a> {
        if let ExpectedType::Specific(fast_expt) = expect {
            if self.extract_type() != fast_expt {
                panic!(
                    "[[Invalid]] mismatch: doesn't typecheck; got {:?}, expected {:?}",
                    self, fast_expt
                )
            };
        };
        self
    }
}

impl<'a> FastTypedProg<'a> {
    pub fn make_clone_struct_serializer(&self) -> StructSerializer {
        let FastTypedProg::Program(_, _, hash_map, hash_map1, _, _) = self;

        StructSerializer {
            struct_sigs: im::HashMap::from_iter(
                hash_map
                    .into_iter()
                    .map(|(key, val)| (key.to_string(), val.to_slow())),
            ),
            struct_layouts: im::HashMap::from_iter(
                hash_map1
                    .into_iter()
                    .map(|(key, val)| (key.to_string(), val.to_slow())),
            ),
        }
    }
}

impl FastExprType {
    /// Gets a pointer to the ExprType which `self` corresponds to
    ///
    /// This borrow is valid for as long as the borrow to `type_set`
    ///
    /// Unwraps, based on the logic that a FastExprType is only constructed after
    /// inserting a type into a TypeCheckState
    #[inline]
    pub fn to_expr_type_ref<'a>(&self, type_set: &'a TypeCheckState) -> &'a ExprType {
        type_set.to_expr_type_ref(*self).unwrap()
    }

    /// Clones the ExprType which `self` corresponds to
    ///
    /// Unwraps, based on the logic that a FastExprType is only constructed after
    /// inserting a type into a TypeCheckState
    #[inline]
    pub fn to_expr_type(&self, type_set: &TypeCheckState) -> ExprType {
        type_set.to_expr_type_ref(*self).unwrap().to_owned()
    }
}

pub struct WrappedFastExpr<'a, 'b> {
    pub fast_expr: FastExpr<'a>,
    pub expected_type: ExpectedType,

    pub type_bindings: im::HashMap<&'a str, FastExprType>, // changes, not shared

    pub struct_hm: &'b im::HashMap<&'a str, i32>,
    pub function_sigs: &'b im::HashMap<&'a str, (FastFunSignature<'a>, ExprType)>, // unchanging; maps function name to tuple of (Sig, PtrType)
    pub struct_sigs: &'b im::HashMap<&'a str, FastStructSignature<'a>>,            // unchanging
    pub allow_input: bool,
}

impl<'a, 'b> WrappedFastExpr<'a, 'b> {
    #[inline]
    pub fn split_child_with_bindings(
        &self,
        expr: FastExpr<'a>,
        expect: ExpectedType,
        bindings: im::HashMap<&'a str, FastExprType>,
    ) -> WrappedFastExpr<'a, 'b> {
        WrappedFastExpr {
            fast_expr: expr,
            struct_hm: self.struct_hm,
            expected_type: expect,
            type_bindings: bindings,
            function_sigs: self.function_sigs,
            struct_sigs: self.struct_sigs,
            allow_input: self.allow_input,
        }
    }

    #[inline]
    pub fn split_child_inherit(
        mut self,
        expr: FastExpr<'a>,
        expect: ExpectedType,
    ) -> WrappedFastExpr<'a, 'b> {
        self.fast_expr = expr;
        self.expected_type = expect;
        self
    }

    #[inline]
    pub fn split_child(&self, expr: FastExpr<'a>, expect: ExpectedType) -> WrappedFastExpr<'a, 'b> {
        self.split_child_with_bindings(expr, expect, self.type_bindings.clone())
    }
}
