fn bad<T, T2>(_exprs: Vec<T>, _state: &mut T2) -> T {
    panic!("constructor already consumed")
}

pub enum StackRetval<From, To> {
    Done(To),
    KeepGoing(From),
}

pub struct StackState<'a, From, To, State> {
    unparsed: Vec<From>,
    parsed: Vec<To>,
    constructor: Box<dyn FnOnce(Vec<To>, &mut State) -> To + 'a>,
    consumer: Option<Box<dyn FnMut(&To, &mut State) -> Option<From> + 'a>>,
}

impl<'a, From, To: 'a, State: 'a> StackState<'a, From, To, State> {
    pub fn new_plus(
        mut unparsed: Vec<From>, // `From` to parse, starting with the 0th element
        constructor: impl FnOnce(Vec<To>, &mut State) -> To + 'a,
        consumer: impl FnMut(&To, &mut State) -> Option<From> + 'a,
    ) -> StackState<'a, From, To, State> {
        unparsed.reverse(); // so that popping still scans Left to Right

        if unparsed.is_empty() {
            panic!("expected non-zero parsing to do")
        }

        StackState {
            unparsed,
            parsed: Vec::new(),
            constructor: Box::new(constructor),
            consumer: Some(Box::new(consumer)),
        }
    }

    pub fn new(
        mut unparsed: Vec<From>, // `From` to parse, starting with the 0th element
        constructor: impl FnOnce(Vec<To>, &mut State) -> To + 'a,
    ) -> StackState<'a, From, To, State> {
        unparsed.reverse(); // so that popping still scans Left to Right

        if unparsed.is_empty() {
            panic!("expected non-zero parsing to do")
        }

        StackState {
            unparsed,
            parsed: Vec::new(),
            constructor: Box::new(constructor),
            consumer: None,
        }
    }

    // /// Consume an expression; None if not enough subexpressions to construct, Some if constructed Expr
    // fn consume(&mut self, expr: To) -> StackRetval<From, To> {
    //     self.parsed.push(expr);

    //     match self.unparsed.pop() {
    //         None => {
    //             let constructor = std::mem::replace(&mut self.constructor, Box::new(bad));
    //             StackRetval::Done(constructor(std::mem::take(&mut self.parsed)))
    //         }
    //         Some(parse_next) => StackRetval::KeepGoing(parse_next),
    //     }
    // }

    fn consume_plus(&mut self, expr: To, state: &mut State) -> StackRetval<From, To> {
        if let Some(ref mut consume_fn) = self.consumer {
            if let Some(append_to_unparsed) = consume_fn(&expr, state) {
                self.unparsed.push(append_to_unparsed);
            }
        }

        self.parsed.push(expr);

        match self.unparsed.pop() {
            None => {
                let constructor = std::mem::replace(&mut self.constructor, Box::new(bad));
                StackRetval::Done(constructor(std::mem::take(&mut self.parsed), state))
            }
            Some(parse_next) => StackRetval::KeepGoing(parse_next),
        }
    }

    fn get_next(&mut self) -> StackRetval<From, To> {
        StackRetval::KeepGoing(self.unparsed.pop().unwrap())
    }
}

pub enum StepResult<'a, From, To, State> {
    Terminal(To),
    Nonterminal(StackState<'a, From, To, State>),
}

pub trait OneStep<'a, To, State> {
    fn step(self, stack_state: &mut State) -> StepResult<'a, Self, To, State>
    where
        Self: Sized;
}

pub struct IterativeStack<'a, From: OneStep<'a, To, State>, To, State> {
    stack: Vec<StackState<'a, From, To, State>>,
    global_state: &'a mut State,
}

impl<'a, From: OneStep<'a, To, State>, To: 'a, State: 'a> IterativeStack<'a, From, To, State> {
    pub fn new(start_state: &'a mut State) -> IterativeStack<'a, From, To, State> {
        IterativeStack {
            stack: Vec::new(),
            global_state: start_state,
        }
    }

    fn push_state(
        &mut self,
        mut to_push: StackState<'a, From, To, State>,
    ) -> StackRetval<From, To> {
        let parse_next = to_push.get_next();
        self.stack.push(to_push);
        parse_next
    }

    /// Consume an expression; None if not done parsing, or Some if done
    fn consume(&mut self, expr: To) -> StackRetval<From, To> {
        let mut to_consume = expr;
        loop {
            let mut curr_context: StackState<From, To, State> = match self.stack.pop() {
                None => return StackRetval::Done(to_consume), // this isn't a subexpression, it's the final expression
                Some(state) => state,
            };

            let context_sub_res = curr_context.consume_plus(to_consume, &mut self.global_state);
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
            let parse_subres = currently_processing.step(self.global_state);
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
