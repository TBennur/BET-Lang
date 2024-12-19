use std::usize;

use crate::lex::{Atom, Lexpr};

#[derive(Debug)]
struct GrowSlice {
    start_ind: usize,
    len: usize,
}

impl GrowSlice {
    fn to_slice<'a, T>(&self, arr: &'a Vec<T>) -> &'a [T] {
        let start = self.start_ind;
        let end = self.start_ind + self.len;
        &arr[start..end]
    }

    fn len(&self) -> usize {
        self.len
    }

    fn new_empty(start_ind: usize) -> GrowSlice {
        GrowSlice {
            start_ind, // inclusive
            len: 0,
        }
    }

    // push ind to the back of the GrowSlice
    fn grow_one(&mut self, ind: usize) -> Result<(), String> {
        if self.len == 0 {
            self.len = 1;
            self.start_ind = ind;
            return Ok(());
        }

        // can only push if last index is previous index
        let next_ind = self.start_ind + self.len;
        if next_ind != ind {
            return Err(format!("tried to push index {ind} to {:?}", self));
        }
        self.len += 1;
        Ok(())
    }

    // pop the ind at the back of the GrowSlice
    fn shrink_one(&mut self) -> Result<usize, String> {
        if self.len > 0 {
            let last_ind = self.start_ind + self.len - 1;
            self.len -= 1;
            Ok(last_ind)
        } else {
            Err(format!("Tried to pop end of {:?} when it's empty", self))
        }
    }
}

mod lex_config {

    use super::AsciiMap;
    type Flag = u8;
    const EMPTY_FLAG: Flag = 0;
    const IS_OPEN_FLAG: Flag = 1 << 0;
    const IS_CLOSE_FLAG: Flag = 1 << 1;
    const IS_DELIM_FLAG: Flag = 1 << 2;
    const IS_IGNORE_FLAG: Flag = 1 << 3;
    const IS_OP_START_FLAG: Flag = 1 << 4;
    const IS_UNARY_OP_FLAG: Flag = 1 << 5;
    const IS_COMMENT_OPEN_FLAG: Flag = 1 << 6;
    const IS_COMMENT_CLOSE_FLAG: Flag = 1 << 7;

    #[derive(Debug)]
    pub struct LexerConfig {
        unaryops: Vec<u8>,
        operators: Vec<Vec<u8>>,
        ignore: Vec<u8>,
        pub open: Vec<u8>,
        pub closing: Vec<u8>,
        pub delims: Vec<u8>,
        pub default_delim: u8,
        comment_open: u8,
        comment_closing: u8,
    }

    impl Default for LexerConfig {
        fn default() -> Self {
            Self {
                // unary operators stick to the next thing pushed-- the next string if a
                // non-empty string is pushed next, or the next list if a list is pushed next
                unaryops: vec![b'~', b'!'],

                // we split on operators, but they don't start a new list
                operators: vec![
                    "->".into(),
                    "::".into(), // type annotation
                    "==".into(), // strict equality
                    ">=".into(),
                    "<=".into(),
                    ":=".into(), // let bindings or set
                    ".".into(),  // update / access structs,
                    // ops
                    "+".into(),
                    "-".into(),
                    "*".into(),
                    "<".into(),
                    ">".into(),
                    "||".into(),
                    "&&".into(),
                ],
                ignore: vec![b' ', b'\t', b'\n'],
                open: vec![b'(', b'{', b'['],
                closing: vec![b')', b'}', b']'],
                delims: vec![
                    b',', b';', /* array access must only have one entry-- no delims */ b'\0',
                ],
                default_delim: b';', // the delim to use when not in any of open
                comment_open: b'#',
                comment_closing: b'\n',
            }
        }
    }


    #[derive(Debug)]
    pub struct FastConfig {
        flags_map: AsciiMap<Flag>, // maps a char to its flags
        open_ind_map: AsciiMap<usize>,
        close_ind_map: AsciiMap<usize>,
        operators: Vec<Vec<u8>>,
        pub open: Vec<u8>,
        pub closing: Vec<u8>,
        pub delims: Vec<u8>,
        pub default_delim: u8,
    }

    impl Default for FastConfig {
        fn default() -> FastConfig {
            FastConfig::new(LexerConfig::default())
        }
    }

    impl FastConfig {
        pub fn new(conf: LexerConfig) -> Self {
            let mut char_flags: [Flag; 256] = [EMPTY_FLAG; 256];
            for ch in conf.unaryops {
                char_flags[ch as usize] |= IS_UNARY_OP_FLAG;
            }
            for ch in conf.ignore {
                char_flags[ch as usize] |= IS_IGNORE_FLAG;
            }
            for &ch in conf.open.as_slice() {
                char_flags[ch as usize] |= IS_OPEN_FLAG;
            }
            for &ch in conf.closing.as_slice() {
                char_flags[ch as usize] |= IS_CLOSE_FLAG;
            }
            for &ch in conf.delims.as_slice() {
                char_flags[ch as usize] |= IS_DELIM_FLAG;
            }
            for op in &conf.operators {
                assert!(op.len() > 0);
                char_flags[op[0] as usize] |= IS_OP_START_FLAG;
            }

            char_flags[conf.comment_open as usize] |= IS_COMMENT_OPEN_FLAG;
            char_flags[conf.comment_closing as usize] |= IS_COMMENT_CLOSE_FLAG;

            let k_v_pairs = char_flags
                .iter()
                .enumerate()
                .filter(|(_ch, flag)| **flag != EMPTY_FLAG)
                .map(|(ch, flag)| (ch as u8, *flag as Flag))
                .collect();

            let flags_map = AsciiMap::new(k_v_pairs);
            let close_ind_map = AsciiMap::new(
                conf.closing
                    .iter()
                    .enumerate()
                    .map(|(ind, &val)| (val, ind))
                    .collect(),
            );
            let open_ind_map = AsciiMap::new(
                conf.open
                    .iter()
                    .enumerate()
                    .map(|(ind, &val)| (val, ind))
                    .collect(),
            );
            Self {
                flags_map,
                operators: conf.operators,
                open: conf.open,
                closing: conf.closing,
                delims: conf.delims,
                default_delim: conf.default_delim,
                open_ind_map,
                close_ind_map,
            }
        }

        pub fn is_open(&self, ch: u8) -> bool {
            match self.flags_map.get(ch) {
                None => false,
                Some(&flag) => (flag & IS_OPEN_FLAG) != EMPTY_FLAG,
            }
        }

        pub fn is_unary_op(&self, ch: u8) -> bool {
            match self.flags_map.get(ch) {
                None => false,
                Some(&flag) => (flag & IS_UNARY_OP_FLAG) != EMPTY_FLAG,
            }
        }

        pub fn is_ignore(&self, ch: u8) -> bool {
            match self.flags_map.get(ch) {
                None => false,
                Some(&flag) => (flag & IS_IGNORE_FLAG) != EMPTY_FLAG,
            }
        }

        pub fn is_comment_close(&self, ch: u8) -> bool {
            match self.flags_map.get(ch) {
                None => false,
                Some(&flag) => (flag & IS_COMMENT_CLOSE_FLAG) != EMPTY_FLAG,
            }
        }

        pub fn is_comment_open(&self, ch: u8) -> bool {
            match self.flags_map.get(ch) {
                None => false,
                Some(&flag) => (flag & IS_COMMENT_OPEN_FLAG) != EMPTY_FLAG,
            }
        }

        pub fn is_close(&self, ch: u8) -> bool {
            match self.flags_map.get(ch) {
                None => false,
                Some(&flag) => (flag & IS_CLOSE_FLAG) != EMPTY_FLAG,
            }
        }

        pub fn is_delim(&self, ch: u8) -> bool {
            match self.flags_map.get(ch) {
                None => false,
                Some(&flag) => (flag & IS_DELIM_FLAG) != EMPTY_FLAG,
            }
        }

        pub fn is_op_start(&self, ch: u8) -> bool {
            match self.flags_map.get(ch) {
                None => false,
                Some(&flag) => (flag & IS_OP_START_FLAG) != EMPTY_FLAG,
            }
        }

        pub fn is_potential_operator(&self, op_so_far: impl AsRef<[u8]>) -> bool {
            super::is_prefix_of_any(op_so_far, &self.operators)
        }

        pub fn open_ind_of(&self, ch: u8) -> Option<usize> {
            self.open_ind_map.get(ch).copied()
        }
        pub fn close_ind_of(&self, ch: u8) -> Option<usize> {
            self.close_ind_map.get(ch).copied()
        }
    }
}

use lex_config::{FastConfig, LexerConfig};

#[derive(Default, Debug)]
pub struct Lexer {
    conf: lex_config::FastConfig,
    stack: Vec<LexState>,
    state: LexState,
}

impl Lexer {
    pub fn new(conf: LexerConfig) -> Self {
        Lexer {
            conf: FastConfig::new(conf),
            stack: Vec::new(),
            state: LexState::default(),
        }
    }

    pub fn lex_fast(&mut self, s: Vec<u8>) -> Lexpr {
        for (i, ch) in s.iter().enumerate() {
            let ch = *ch;

            if !ch.is_ascii() {
                panic!("Invalid: bad char: {ch}")
            }

            if self.state.in_comment {
                if self.conf.is_comment_close(ch) {
                    self.state.in_comment = false;
                }
                continue;
            }

            match ch {
                ch if self.conf.is_ignore(ch) => {
                    self.state.split_partial_str(&s, i + 1);
                }

                ch if self.conf.is_comment_open(ch) => {
                    self.state.in_comment = true;
                    continue;
                }

                ch if self.conf.is_open(ch) => {
                    self.state.split_partial_str(&s, i + 1);

                    // make new context
                    let curr_opening_ind = self.conf.open_ind_of(ch).unwrap();
                    let new_context = LexState::new(Some(curr_opening_ind));

                    // push old context
                    let old_context = std::mem::replace(&mut self.state, new_context);
                    assert!(!old_context.in_comment);
                    self.stack.push(old_context);
                }

                ch if self.conf.is_close(ch) => {
                    let closing_index = self.conf.close_ind_of(ch).unwrap();

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
                    self.state.split_partial_list(true, &s, i);

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
                                    b'{' => Lexpr::CurlyList(old_state.context),
                                    b'(' => Lexpr::ParenList(old_state.context),
                                    b'[' => Lexpr::BraceList(old_state.context),
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
                    self.state.split_partial_list(false, &s, i);
                }

                ch if self.conf.is_unary_op(ch) => {
                    self.state.encounter_unary_op(ch, i + 1);
                    continue;
                }

                ch => {
                    // if we were looking for an operator, check if we can continue building an operator with the current char (always prefer longer operators)
                    if self.state.finding_operator {
                        self.state.partial_str.grow_one(i).unwrap();
                        let with_next_char = self.state.partial_str.to_slice(&s);
                        if self.conf.is_potential_operator(with_next_char) {
                            // continue building operator
                            continue;
                        }
                        self.state.partial_str.shrink_one().unwrap(); // undo push
                                                                      // give up on building operator; split off partial string
                        self.state.split_partial_str(&s, i);
                        // fallthrough to process ch
                    }

                    // check if this is a start of the operator
                    if self.conf.is_op_start(ch) {
                        assert!(!self.state.finding_operator); // as if it was, we either split the partial_str (setting to false) or continued
                        self.state.split_partial_str(&s, i);
                        self.state.partial_str.grow_one(i).unwrap();
                        self.state.finding_operator = true;
                        continue;
                    }

                    assert!(!self.state.finding_operator);
                    self.state.partial_str.grow_one(i).unwrap();
                }
            }
        }

        // we expect that there's nothing on the stack
        if self.stack.len() != 0 {
            panic!("Invalid: open and closing don't match");
        }
        // panic!("{:?}", state.context);
        self.state.split_partial_list(true, &s, 0); // implicitly the entire str is wrapped in {}

        Lexpr::List(std::mem::take(&mut self.state.context))
    }
}

#[derive(Debug)]
pub struct AsciiMap<T> {
    vals: [Option<T>; 256],
}

impl<T: std::marker::Copy> AsciiMap<T> {
    fn get(&self, ch: u8) -> Option<&T> {
        self.vals[ch as usize].as_ref()
    }
    fn set(&mut self, ch: u8, val: T) {
        self.vals[ch as usize] = Some(val);
    }
    fn del(&mut self, ch: u8) {
        self.vals[ch as usize] = None;
    }

    fn new(k_v_pairs: Vec<(u8, T)>) -> AsciiMap<T> {
        let mut res = AsciiMap::<T> { vals: [None; 256] };

        for (k, v) in k_v_pairs {
            res.vals[k as usize] = Some(v);
        }
        res
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

#[derive(Debug)]
struct LexState {
    active_unary_ops: Option<Vec<u8>>, // NONE or the unary op we have
    opening_ind: Option<usize>,        // NONE or index
    context: Vec<Lexpr>, // a context, of a type corresponding to curr_opening_ind, such as an entire {}
    partial_list: Vec<Lexpr>, // the list which composes the current list, such as
    partial_str: GrowSlice,
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
            partial_str: GrowSlice::new_empty(0),
            finding_operator: false,
            in_comment: false,
        }
    }

    fn split_partial_str(&mut self, backing: &Vec<u8>, next_ind: usize) {
        let partial_str = self.partial_str.to_slice(&backing);
        self.partial_str = GrowSlice::new_empty(next_ind);

        if partial_str.len() > 0 {
            let mut to_push =
                Lexpr::Atom(atom_from(String::from_utf8(partial_str.to_vec()).unwrap()));

            if let Some(ref mut uops) = self.active_unary_ops {
                while uops.len() > 0 {
                    // since we pushed to the back, pop to get most recent
                    let cur_uop = uops.pop().unwrap();
                    let uop_atom = Lexpr::Atom(Atom::S((cur_uop as char).to_string()));
                    let v = vec![uop_atom, to_push];
                    to_push = Lexpr::List(v);
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
                let uop_atom = Lexpr::Atom(Atom::S((cur_uop as char).to_string()));
                to_push = Lexpr::List(vec![uop_atom, to_push]);
            }
            self.active_unary_ops = None;
        }

        self.context.push(to_push);
    }

    /// Add the current list, if it's non-empty, unwrapping it if needed
    fn split_partial_list(&mut self, is_closing: bool, backing: &Vec<u8>, next_ind: usize) {
        self.split_partial_str(backing, next_ind);
        self._split_partial_list(is_closing);
    }

    fn encounter_unary_op(&mut self, uop: u8, ind: usize) {
        match &mut self.active_unary_ops {
            Some(vec) => vec.push(uop),
            None => self.active_unary_ops = Some(vec![uop]),
        };
    }
}

// fn is_prefix_of_any(prefix: &Vec<u8>, vec: &Vec<Vec<u8>>) -> bool {
//     vec.iter().any(|s| s.starts_with(prefix))
// }
pub fn is_prefix_of_any<T>(prefix: impl AsRef<[T]>, vec: &[Vec<T>]) -> bool
where
    T: PartialEq,
{
    vec.iter().any(|s| s.starts_with(prefix.as_ref()))
}
