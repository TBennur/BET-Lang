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
