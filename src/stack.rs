fn bad<T>(_exprs: Vec<T>) -> T {
    panic!("constructor already consumed")
}

enum StackRetval<From, To> {
    Done(To),
    KeepGoing(From),
}

pub struct StackState<'a, From, To> {
    unparsed: Vec<From>,
    parsed: Vec<To>,
    constructor: Box<dyn FnOnce(Vec<To>) -> To + 'a>,
}

impl <'a, From, To: 'a> StackState<'a, From, To> {
    pub fn new(
        mut unparsed: Vec<From>,
        constructor: impl FnOnce(Vec<To>) -> To + 'a,
    ) -> StackState<'a, From, To> {
        unparsed.reverse(); // so that popping still scans Left to Right

        if unparsed.is_empty() {
            panic!("expected non-zero parsing to do")
        }

        StackState {
            unparsed,
            parsed: Vec::new(),
            constructor: Box::new(constructor),
        }
    }

    /// Consume an expression; None if not enough subexpressions to construct, Some if constructed Expr
    fn consume(&mut self, expr: To) -> StackRetval<From, To> {
        self.parsed.push(expr);

        match self.unparsed.pop() {
            None => {
                let constructor = std::mem::replace(&mut self.constructor, Box::new(bad));
                StackRetval::Done(constructor(std::mem::take(&mut self.parsed)))
            }
            Some(parse_next) => StackRetval::KeepGoing(parse_next),
        }
    }

    fn get_next(&mut self) -> StackRetval<From, To> {
        StackRetval::KeepGoing(self.unparsed.pop().unwrap())
    }
}


pub enum StepResult<'a, From, To>  {
    Terminal(To),
    Nonterminal(StackState<'a, From, To>)
}

pub trait OneStep<'a, To> {
    fn step(self) -> StepResult<'a, Self, To>
    where
        Self: Sized;
}

pub struct IterativeStack<'a, From: OneStep<'a, To>, To> {
    stack: Vec<StackState<'a, From, To>>,
}

impl<'a, From: OneStep<'a, To> , To: 'a> IterativeStack<'a, From, To> {
    pub fn new() -> IterativeStack<'a, From, To> {
        IterativeStack { stack: Vec::new() }
    }

    fn push_state(&mut self, mut to_push: StackState<'a, From, To>) -> StackRetval<From, To> {
        let parse_next = to_push.get_next();
        self.stack.push(to_push);
        parse_next
    }

    /// Consume an expression; None if not done parsing, or Some if done
    fn consume(&mut self, expr: To) -> StackRetval<From, To> {
        let mut to_consume = expr;
        loop {
            let mut curr_context: StackState<From, To> = match self.stack.pop() {
                None => return StackRetval::Done(to_consume), // this isn't a subexpression, it's the final expression
                Some(state) => state,
            };

            let context_sub_res = curr_context.consume(to_consume);
            match context_sub_res {
                StackRetval::Done(consume_next) => {
                    to_consume = consume_next;
                }
                StackRetval::KeepGoing(parse_next) => {
                    self.stack.push(curr_context); // this expression needs more, so is still current context
                    return StackRetval::KeepGoing(parse_next);
                }
            }
        }
    }

    pub fn iterate(&mut self, to_process: From) -> To {
        let mut currently_processing = to_process;
        loop {
            let parse_subres = currently_processing.step();
            let parse_subres = match parse_subres {
                StepResult::Terminal(to_consume) => self.consume(to_consume),
                StepResult::Nonterminal(new_stack) => self.push_state(new_stack),
            };
            match parse_subres {
                StackRetval::Done(res) => return res,
                StackRetval::KeepGoing(cont) => currently_processing = cont,
            }
        }
    }
}
