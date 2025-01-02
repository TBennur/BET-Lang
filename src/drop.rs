use crate::structs::{FastTypedExpr, FastTypedExpr::*};

impl<'a> FastTypedExpr<'a> {
    fn move_children_to(&mut self, write_to: &mut Vec<FastTypedExpr<'a>>) {
        match self {
            __INTERNAL
            | Number(_)
            | Boolean(_)
            | Id(_, _)
            | Unit
            | FunName(_, _)
            | Input
            | RDInput
            | Null(_)
            | Alloc(_) => (),

            Let(_, vec, fast_typed_expr) => {
                let vec = std::mem::take(vec);
                for (_, fte) in vec {
                    write_to.push(fte);
                }
                write_to.push(std::mem::take(fast_typed_expr));
            }

            UnOp(_, _, fast_typed_expr) => write_to.push(std::mem::take(fast_typed_expr)),

            BinOp(_, _, fast_typed_expr, fast_typed_expr1) => write_to.extend(
                [fast_typed_expr, fast_typed_expr1]
                    .into_iter()
                    .map(|fte| std::mem::take(&mut **fte)),
            ),

            If(_, fast_typed_expr, fast_typed_expr1, fast_typed_expr2) => write_to.extend(
                [fast_typed_expr, fast_typed_expr1, fast_typed_expr2]
                    .into_iter()
                    .map(|fte| std::mem::take(&mut **fte)),
            ),

            RepeatUntil(_, fast_typed_expr, fast_typed_expr1) => write_to.extend(
                [fast_typed_expr, fast_typed_expr1]
                    .into_iter()
                    .map(|fte| std::mem::take(&mut **fte)),
            ),

            Set(_, _, fast_typed_expr) => write_to.push(std::mem::take(fast_typed_expr)),

            Block(_, vec) => write_to.extend(std::mem::take(vec)),

            Call(_, fast_typed_expr, vec) => {
                write_to.push(std::mem::take(fast_typed_expr));
                write_to.extend(std::mem::take(vec));
            }

            Update(_, fast_typed_expr, _, fast_typed_expr1) => write_to.extend(
                [fast_typed_expr, fast_typed_expr1]
                    .into_iter()
                    .map(|fte| std::mem::take(&mut **fte)),
            ),
            Lookup(_, fast_typed_expr, _) => write_to.push(std::mem::take(fast_typed_expr)),
            ArrayAlloc(_, fast_typed_expr) => write_to.push(std::mem::take(fast_typed_expr)),
            ArrayLookup(_, fast_typed_expr, fast_typed_expr1) => write_to.extend(
                [fast_typed_expr, fast_typed_expr1]
                    .into_iter()
                    .map(|fte| std::mem::take(&mut **fte)),
            ),
            ArrayUpdate(_, fast_typed_expr, fast_typed_expr1, fast_typed_expr2) => write_to.extend(
                [fast_typed_expr, fast_typed_expr1, fast_typed_expr2]
                    .into_iter()
                    .map(|fte| std::mem::take(&mut **fte)),
            ),
            ArrayLen(_, fast_typed_expr) => write_to.push(std::mem::take(fast_typed_expr)),
        }
    }
}

impl<'a> Drop for FastTypedExpr<'a> {
    fn drop(&mut self) {
        let mut to_drop = Vec::new();
        self.move_children_to(&mut to_drop);
        while let Some(mut next) = to_drop.pop() {
            next.move_children_to(&mut to_drop);
        }
    }
}
