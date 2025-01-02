use crate::alt_stack::{StackRetval, StackStateAble};

/* --- Simple Stack State for when all sub-problems are known as soon as the parent is encountered --- */

pub trait Constructor<To> {
    fn construct(&mut self, parsed: &mut Vec<To>) -> To;
}

pub struct SimpleStackState<From, To, C>
where
    C: Constructor<To>,
{
    unparsed: Vec<From>,
    parsed: Vec<To>,
    len: usize,
    constructor: C,
}

impl<From, To, C> SimpleStackState<From, To, C>
where
    C: Constructor<To>,
{
    pub fn new(mut unparsed: Vec<From>, constructor: C) -> Self {
        let len = unparsed.len();

        if len == 0 {
            panic!("Zero parsing to do!")
        }

        unparsed.reverse(); // will pop from back
        SimpleStackState {
            unparsed,
            parsed: Vec::with_capacity(len),
            len,
            constructor,
        }
    }
}

impl<From, To, C> StackStateAble<()> for SimpleStackState<From, To, C>
where
    C: Constructor<To>,
{
    type From = From;
    type To = To;

    fn consume_plus(
        &mut self,
        expr: Self::To,
        _state: &mut (),
    ) -> StackRetval<Self::From, Self::To> {
        self.parsed.push(expr);
        match self.unparsed.pop() {
            None => {
                assert!(self.len == self.parsed.len()); // as SimpleStackState provides no ability to produce unparsed
                StackRetval::Done(self.constructor.construct(&mut self.parsed))
            }
            Some(parse_next) => StackRetval::KeepGoing(parse_next),
        }
    }

    fn get_next(&mut self) -> StackRetval<Self::From, Self::To> {
        StackRetval::KeepGoing(self.unparsed.pop().unwrap())
    }
}

/* --- Stack State Similar to one in stack.rs --- */

pub struct StackState<'a, From, To, State> {
    unparsed: Vec<From>,
    parsed: Vec<To>,
    constructor: Box<dyn FnOnce(Vec<To>, &mut State) -> To + 'a>,
    consumer: Option<Box<dyn FnMut(&To, &mut State) -> Option<From> + 'a>>,
}

impl<'a, From, To: 'a, State: 'a> StackStateAble<State> for StackState<'a, From, To, State> {
    type From = From;
    type To = To;

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

fn bad<T, T2>(_exprs: Vec<T>, _state: &mut T2) -> T {
    panic!("constructor already consumed")
}

impl<'a, From, To: 'a, State: 'a> StackState<'a, From, To, State> {
    #[allow(dead_code)]
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

    #[allow(dead_code)]
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
}
