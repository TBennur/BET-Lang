#[derive(Debug)]
pub enum Atom {
    S(String),
    I(i64),
    F(f64),
}

#[derive(Debug)]
pub enum Lexpr {
    Atom(Atom),
    List(Vec<Lexpr>),
    CurlyList(Vec<Lexpr>),
    ParenList(Vec<Lexpr>),
}

pub struct LexerConfig {
    operators: Vec<&'static str>,
    ignore: Vec<char>,
    open: Vec<char>,
    closing: Vec<char>,
    delims: Vec<char>,
    default_delim: char,
}

impl LexerConfig {
    pub fn default() -> Self {
        Self {
            // we split on operators, but they don't start a new list
            operators: vec![
                "::", // type annotation
                "==", // strict equality
                ">=", "<=", ":=", // let bindings or set
                ".",  // update / access structs,
                // ops
                "+", "-", "*", "<", ">",
            ],
            ignore: vec![' ', '\t', '\n'],
            open: vec!['(', '{'],
            closing: vec![')', '}'],
            delims: vec![',', ';'],
            default_delim: ';', // the delim to use when not in any of open
        }
    }

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
}

struct LexState {
    opening_ind: Option<usize>, // NONE or index
    context: Vec<Lexpr>, // a context, of a type corresponding to curr_opening_ind, such as an entire {}
    partial_list: Vec<Lexpr>, // the list which composes the current list, such as
    partial_str: String,
    finding_operator: bool,
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

impl LexState {
    fn new(opening_ind: Option<usize>) -> LexState {
        LexState {
            opening_ind,
            context: Vec::new(),
            partial_list: Vec::new(),
            partial_str: "".to_string(),
            finding_operator: false,
        }
    }

    fn split_partial_str(&mut self) {
        if self.partial_str.len() > 0 {
            let complete_str = std::mem::replace(&mut self.partial_str, "".to_string());
            self.partial_list.push(Lexpr::Atom(atom_from(complete_str)));
        }
        self.finding_operator = false;
    }

    fn _split_partial_list(&mut self) {
        if self.partial_list.len() == 0 {
            return;
        }

        let completed_list = std::mem::replace(&mut self.partial_list, Vec::new());
        let expr = match completed_list.len() {
            0 => unreachable!(),
            1 => completed_list.into_iter().next().unwrap(),
            _ => Lexpr::List(completed_list),
        };

        self.context.push(expr);
    }

    /// Add the current list, if it's non-empty, unwrapping it if needed
    fn split_partial_list(&mut self) {
        self.split_partial_str();
        self._split_partial_list();
    }
}

fn index_of<T: std::cmp::PartialEq>(v: &Vec<T>, e: T) -> Result<usize, String> {
    match v.iter().position(|eprime| *eprime == e) {
        Some(pos) => Ok(pos),
        None => Err("Elem not found".to_string()),
    }
}

fn is_prefix_of_any(prefix: &String, vec: &Vec<&str>) -> bool {
    vec.iter().any(|s| s.starts_with(prefix))
}

pub fn lex(s: &String, conf: LexerConfig) -> Lexpr {
    let mut stack: Vec<LexState> = Vec::new(); // tuple of index which the context belongs to, and the context
    let mut state = LexState::new(None);

    for (i, ch) in s.char_indices() {
        match ch {
            ch if conf.is_ignore(ch) => {
                state.split_partial_str();
            }

            ch if conf.is_open(ch) => {
                state.split_partial_str();

                // push old context
                stack.push(state);

                // make new context
                let curr_opening_ind = index_of(&conf.open, ch).unwrap();
                state = LexState::new(Some(curr_opening_ind));
            }

            ch if conf.is_close(ch) => {
                let closing_index = index_of(&conf.closing, ch).unwrap();

                if Some(closing_index) != state.opening_ind {
                    let expected = match state.opening_ind {
                        Some(ind) => Some(conf.closing[ind]),
                        None => None,
                    };
                    panic!(
                        "Illegal: closing {ch} at position {i} did not match opening {:?}",
                        expected
                    );
                }

                // we have a valid closing
                state.split_partial_list();

                // pop from previous closures, adding this closure to its partial_list
                match stack.pop() {
                    None => unreachable!("We started with a curr_opening_ind == -1"),
                    Some(updated_state) => {
                        let old_state = std::mem::replace(&mut state, updated_state);
                        assert!(state.partial_str.len() == 0);
                        state.partial_list.push(match conf.open[closing_index] {
                            '{' => Lexpr::CurlyList(old_state.context),
                            '(' => Lexpr::ParenList(old_state.context),
                            _ => unreachable!(),
                        });
                    }
                }
            }

            ch if conf.is_delim(ch) => {
                let expected_delim = match state.opening_ind {
                    Some(expected_ind) => conf.delims[expected_ind],
                    None => conf.default_delim,
                };
                if expected_delim != ch {
                    panic!("Mismatched delimiter: got {ch} when we expected {expected_delim}");
                }

                // delimiter matches; split off the partial_string, then the partial_list
                state.split_partial_list();
            }

            ch if ch.is_ascii() => {
                // if we were looking for an operator, check if we can continue building an operator with the current char (always prefer longer operators)
                if state.finding_operator {
                    let mut op_so_far = state.partial_str.to_string();
                    op_so_far.push(ch);
                    if conf.is_potential_operator(&op_so_far) {
                        // continue building
                        state.partial_str.push(ch);
                        continue;
                    }
                    // give up on building operator; split off partial string
                    state.split_partial_str();
                    // fallthrough to process ch
                }

                // check if this is a start of the operator
                if conf.is_op_start(ch) {
                    assert!(!state.finding_operator); // as if it was, we either split the partial_str (setting to false) or continued
                    state.split_partial_str();
                    state.partial_str.push(ch);
                    state.finding_operator = true;
                    continue;
                }

                assert!(!state.finding_operator);
                state.partial_str.push(ch);
            }

            _ => panic!("Invalid: bad char: {ch}"),
        }
    }

    // we expect that there's nothing on the stack
    if stack.len() != 0 {
        panic!("Invalid: open and closing don't match");
    }
    state.split_partial_list();
    Lexpr::List(state.context)
}
