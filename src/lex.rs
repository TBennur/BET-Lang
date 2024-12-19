#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'a> {
    S(&'a str),
    I(i64),
    F(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lexpr<'a> {
    Atom(Atom<'a>),
    List(Vec<Lexpr<'a>>),
    CurlyList(Vec<Lexpr<'a>>),
    ParenList(Vec<Lexpr<'a>>),
    BraceList(Vec<Lexpr<'a>>),
    Stolen, // special fake lexpr which the parser creates when taking
}

impl<'a> Default for Lexpr<'a> {
    fn default() -> Self {
        Lexpr::Stolen
    }
}
