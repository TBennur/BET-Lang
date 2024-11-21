// Strips comments
pub fn strip(s: &String, comment_open: char, comment_closing: char) -> String {
    let mut is_comment = false;
    let mut stripped_prog = String::new();
    for (_i, ch) in s.char_indices() {
        if ch == comment_open {
            is_comment = true;
        } else if is_comment && (ch == comment_closing) {
            is_comment = false;
        } else if !is_comment {
            stripped_prog.push(ch)
        }
    }
    stripped_prog
}
/*
def removeComments(snek_program):
    """
    Remove comments from snek file by discarding text between ; and \n
    Simulate mini FSM
    """
    mode = "append"
    newS = []
    for c in snek_program:
        if c == ";":
            mode = "skip"
        if mode == "skip" and c == "\n":
            mode = "append"
        if mode == "append":
            newS.append(c)
    return "".join(newS)
*/
