pub trait StackStateAble<State> {
    type From;
    type To;

    fn consume_plus(
        &mut self,
        expr: Self::To,
        state: &mut State,
    ) -> StackRetval<Self::From, Self::To>;
    fn get_next(&mut self) -> StackRetval<Self::From, Self::To>;
}

pub enum StackRetval<Temp, To> {
    Done(To),
    KeepGoing(Temp),
}

pub enum StepResult<To, Temp> {
    Terminal(To),
    Nonterminal(Temp),
}

pub trait OneStep<State, Temp>
where
    Temp: StackStateAble<State>,
    Self: Sized,
{
    fn step(self, stack_state: &mut State) -> StepResult<Temp::To, Temp>;
}

pub struct IterativeStack<'a, Temp, State> {
    stack: Vec<Temp>,
    global_state: &'a mut State,
}

impl<'a, State, Temp> IterativeStack<'a, Temp, State>
where
    Temp: StackStateAble<State>,
    Temp::From: OneStep<State, Temp>,
{
    pub fn new(start_state: &'a mut State) -> IterativeStack<'a, Temp, State> {
        IterativeStack {
            stack: Vec::new(),
            global_state: start_state,
        }
    }

    fn push_state(&mut self, mut to_push: Temp) -> StackRetval<Temp::From, Temp::To> {
        let parse_next = to_push.get_next();
        self.stack.push(to_push);
        parse_next
    }

    /// Consume an expression; None if not done parsing, or Some if done
    fn consume(&mut self, expr: Temp::To) -> StackRetval<Temp::From, Temp::To> {
        let mut to_consume = expr;
        loop {
            let mut curr_context: Temp = match self.stack.pop() {
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

    pub fn iterate(&mut self, to_process: Temp::From) -> Temp::To {
        let mut currently_processing: Temp::From = to_process;
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
