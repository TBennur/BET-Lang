#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    S(String),
    I(i64),
    F(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lexpr {
    Atom(Atom),
    List(Vec<Lexpr>),
    CurlyList(Vec<Lexpr>),
    ParenList(Vec<Lexpr>),
    BraceList(Vec<Lexpr>),
    Stolen, // special fake lexpr which the parser creates when taking
}

impl Default for Lexpr {
    fn default() -> Self {
        Lexpr::Stolen
    }
}

#[derive(Default)]
pub struct Lexer {
    conf: LexerConfig,
    stack: Vec<LexState>,
    state: LexState,
}

impl Lexer {
    /*
    pub fn new(conf: LexerConfig) -> Self {
        Lexer {
            conf,
            stack: Vec::new(),
            state: LexState::default(),
        }
    }
    */

    pub fn lex(&mut self, s: &String) -> Lexpr {
        for (i, ch) in s.char_indices() {
            if self.state.in_comment {
                if self.conf.is_comment_close(ch) {
                    self.state.in_comment = false;
                }
                continue;
            }

            match ch {
                ch if self.conf.is_ignore(ch) => {
                    self.state.split_partial_str();
                }

                ch if self.conf.is_comment_open(ch) => {
                    self.state.in_comment = true;
                    continue;
                }

                ch if self.conf.is_open(ch) => {
                    self.state.split_partial_str();

                    // make new context
                    let curr_opening_ind = index_of(&self.conf.open, ch).unwrap();
                    let new_context = LexState::new(Some(curr_opening_ind));

                    // push old context
                    let old_context = std::mem::replace(&mut self.state, new_context);
                    assert!(!old_context.in_comment);
                    self.stack.push(old_context);
                }

                ch if self.conf.is_close(ch) => {
                    let closing_index = index_of(&self.conf.closing, ch).unwrap();

                    if Some(closing_index) != self.state.opening_ind {
                        let expected = match self.state.opening_ind {
                            Some(ind) => Some(self.conf.closing[ind]),
                            None => None,
                        };
                        panic!(
                            "Illegal: closing {ch} at position {i} did not match opening {:?}",
                            expected
                        );
                    }

                    // we have a valid closing
                    self.state.split_partial_list(true);

                    // pop from previous closures, adding this closure to its partial_list
                    match self.stack.pop() {
                        None => unreachable!("We started with a curr_opening_ind == -1"),
                        Some(updated_state) => {
                            let old_state = std::mem::replace(&mut self.state, updated_state);
                            if let Some(vec) = old_state.active_unary_ops {
                                assert!(vec.len() > 0);
                                panic!("Had unary ops which weren't bound: {:?}", vec)
                            }

                            assert!(self.state.partial_str.len() == 0);
                            self.state
                                .partial_list
                                .push(match self.conf.open[closing_index] {
                                    '{' => Lexpr::CurlyList(old_state.context),
                                    '(' => Lexpr::ParenList(old_state.context),
                                    '[' => Lexpr::BraceList(old_state.context),
                                    _ => unreachable!(),
                                });
                        }
                    }
                }

                ch if self.conf.is_delim(ch) => {
                    let expected_delim = match self.state.opening_ind {
                        Some(expected_ind) => self.conf.delims[expected_ind],
                        None => self.conf.default_delim,
                    };
                    if expected_delim != ch {
                        panic!("Mismatched delimiter: got {ch} when we expected {expected_delim}");
                    }

                    // delimiter matches; split off the partial_string, then the partial_list
                    self.state.split_partial_list(false);
                }

                ch if self.conf.is_unary_op(ch) => {
                    self.state.encounter_unary_op(ch);
                    continue;
                }

                ch if ch.is_ascii() => {
                    // if we were looking for an operator, check if we can continue building an operator with the current char (always prefer longer operators)
                    if self.state.finding_operator {
                        self.state.partial_str.push(ch);
                        if self.conf.is_potential_operator(&self.state.partial_str) {
                            // continue building operator
                            continue;
                        }
                        self.state.partial_str.pop().unwrap(); // undo push
                                                               // give up on building operator; split off partial string
                        self.state.split_partial_str();
                        // fallthrough to process ch
                    }

                    // check if this is a start of the operator
                    if self.conf.is_op_start(ch) {
                        assert!(!self.state.finding_operator); // as if it was, we either split the partial_str (setting to false) or continued
                        self.state.split_partial_str();
                        self.state.partial_str.push(ch);
                        self.state.finding_operator = true;
                        continue;
                    }

                    assert!(!self.state.finding_operator);
                    self.state.partial_str.push(ch);
                }

                _ => panic!("Invalid: bad char: {ch}"),
            }
        }

        // we expect that there's nothing on the stack
        if self.stack.len() != 0 {
            panic!("Invalid: open and closing don't match");
        }
        // panic!("{:?}", state.context);
        self.state.split_partial_list(true); // implicitly the entire str is wrapped in {}

        Lexpr::List(std::mem::take(&mut self.state.context))
    }
}

pub struct LexerConfig {
    unaryops: Vec<char>,
    operators: Vec<&'static str>,
    ignore: Vec<char>,
    open: Vec<char>,
    closing: Vec<char>,
    delims: Vec<char>,
    default_delim: char,
    comment_open: char,
    comment_closing: char,
}

impl Default for LexerConfig {
    fn default() -> Self {
        Self {
            // unary operators stick to the next thing pushed-- the next string if a
            // non-empty string is pushed next, or the next list if a list is pushed next
            unaryops: vec!['~', '!'],

            // we split on operators, but they don't start a new list
            operators: vec![
                "->", "::", // type annotation
                "==", // strict equality
                ">=", "<=", ":=", // let bindings or set
                ".",  // update / access structs,
                // ops
                "+", "-", "*", "<", ">", "||", "&&",
            ],
            ignore: vec![' ', '\t', '\n'],
            open: vec!['(', '{', '['],
            closing: vec![')', '}', ']'],
            delims: vec![
                ',', ';', /* array access must only have one entry-- no delims */ '\0',
            ],
            default_delim: ';', // the delim to use when not in any of open
            comment_open: '#',
            comment_closing: '\n',
        }
    }
}

impl LexerConfig {
    fn is_ignore(&self, ch: char) -> bool {
        self.ignore.contains(&ch)
    }

    fn is_open(&self, ch: char) -> bool {
        self.open.contains(&ch)
    }

    fn is_close(&self, ch: char) -> bool {
        self.closing.contains(&ch)
    }

    fn is_delim(&self, ch: char) -> bool {
        self.delims.contains(&ch)
    }

    fn is_potential_operator(&self, op_so_far: &String) -> bool {
        is_prefix_of_any(op_so_far, &self.operators)
    }

    fn is_op_start(&self, ch: char) -> bool {
        for op in &self.operators {
            assert!(op.len() > 0);
            if op.chars().nth(0).unwrap() == ch {
                return true;
            };
        }
        false
    }

    fn is_unary_op(&self, ch: char) -> bool {
        self.unaryops.contains(&ch)
    }

    fn is_comment_open(&self, ch: char) -> bool {
        self.comment_open == ch
    }

    fn is_comment_close(&self, ch: char) -> bool {
        self.comment_closing == ch
    }
}

fn atom_from(s: String) -> Atom {
    if let Ok(x) = s.parse() {
        return Atom::I(x);
    };
    if let Ok(x) = s.parse() {
        return Atom::F(x);
    };

    Atom::S(s)
}

struct LexState {
    active_unary_ops: Option<Vec<char>>, // NONE or the unary op we have
    opening_ind: Option<usize>,          // NONE or index
    context: Vec<Lexpr>, // a context, of a type corresponding to curr_opening_ind, such as an entire {}
    partial_list: Vec<Lexpr>, // the list which composes the current list, such as
    partial_str: String,
    finding_operator: bool,
    in_comment: bool,
}

impl Default for LexState {
    fn default() -> Self {
        LexState::new(None)
    }
}

impl LexState {
    fn new(opening_ind: Option<usize>) -> LexState {
        LexState {
            active_unary_ops: None,
            opening_ind,
            context: Vec::with_capacity(1),
            partial_list: Vec::new(),
            partial_str: "".to_string(),
            finding_operator: false,
            in_comment: false,
        }
    }

    fn split_partial_str(&mut self) {
        if self.partial_str.len() > 0 {
            let complete_str = std::mem::replace(&mut self.partial_str, "".to_string());
            let str_atom = Lexpr::Atom(atom_from(complete_str));
            let mut to_push = str_atom;

            if let Some(ref mut uops) = self.active_unary_ops {
                while uops.len() > 0 {
                    // since we pushed to the back, pop to get most recent
                    let cur_uop = uops.pop().unwrap();
                    let uop_atom = Lexpr::Atom(Atom::S(cur_uop.to_string()));
                    to_push = Lexpr::List(vec![uop_atom, to_push]);
                }
                self.active_unary_ops = None;
            }

            self.partial_list.push(to_push);
        }
        self.finding_operator = false;
    }

    fn _split_partial_list(&mut self, is_closing: bool) {
        if is_closing && self.context.len() == 0 && self.partial_list.len() == 0 {
            // so that {} => CurlyList([]), () => ParenList([])
            return;
        }

        let mut completed_list = std::mem::replace(&mut self.partial_list, Vec::new());
        let mut to_push = match completed_list.len() {
            0 => Lexpr::List(vec![]), // allow for unit type in blocks
            1 => completed_list.pop().unwrap(),
            _ => Lexpr::List(completed_list),
        };

        if let Some(ref mut uops) = self.active_unary_ops {
            while uops.len() > 0 {
                // since we pushed to the back, pop to get most recent
                let cur_uop = uops.pop().unwrap();
                let uop_atom = Lexpr::Atom(Atom::S(cur_uop.to_string()));
                to_push = Lexpr::List(vec![uop_atom, to_push]);
            }
            self.active_unary_ops = None;
        }

        self.context.push(to_push);
    }

    /// Add the current list, if it's non-empty, unwrapping it if needed
    fn split_partial_list(&mut self, is_closing: bool) {
        self.split_partial_str();
        self._split_partial_list(is_closing);
    }

    fn encounter_unary_op(&mut self, uop: char) {
        match &mut self.active_unary_ops {
            Some(vec) => vec.push(uop),
            None => self.active_unary_ops = Some(vec![uop]),
        };
    }
}

fn index_of<T: std::cmp::PartialEq>(v: &Vec<T>, e: T) -> Result<usize, String> {
    match v.iter().position(|eprime| *eprime == e) {
        Some(pos) => Ok(pos),
        None => Err("Elem not found".to_string()),
    }
}

fn is_prefix_of_any(prefix: &String, vec: &Vec<&str>) -> bool {
    vec.iter().any(|s: &&str| s.starts_with(prefix))
}
