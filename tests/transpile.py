# This should approximately transpile .snek files to .?? files
# All transpiled files should be manually checked for correctness
# This overall structure, expecially unwrapNlet and format assignations, can be converted for parsing
# However, it does strip comments

import os

OPEN_BRACE = "{"
CLOSE_BRACE = "}"
NEWLINE = "\n"
MATH_OPERATORS = "<>=+-*"
UNOPS = ["add1", "sub1", "print"]
BINOPS = ["+", "-", "*", "<", ">", ">=", "<="]
TARGET_DIRECTORIES = ["boa", "cobra", "diamondback", "eggeater", "input"]

# converts a (b) c into [a, (b), c]
# Uses tracers in a mini fsm
def unwrapNlet(s, N, keyword = None):
    # Setup tracer
    parts = []
    start = 0
    cur = 0
    tot = 0
    mode = "any"
    
    # Iterate
    for c in s:
        cur += 1
        if N is not None and len(parts) == N - 1:
            break
        if mode == "paren":
            if c == "(":
                tot += 1
            elif c == ")":
                tot -= 1
                if tot == 0:
                    parts.append(s[start:cur].strip())
                    start = cur 
                    mode = "any"
        elif mode == "spaced":
            if c == " ":
                parts.append(s[start:cur-1].strip())
                start = cur
                mode = "any"
            if tot == 0 and c == "(":
                parts.append(s[start:cur-1].strip())
                start = cur-1
                tot = 1
                mode = "paren"
        else:
            assert(mode == "any")
            assert(tot == 0)
            if c == "(":
                mode = "paren"
                tot += 1
            elif c.isalnum() or c in MATH_OPERATORS:
                mode = "spaced"
    
    # Add final entry
    end = s[start:].strip()
    if end != "":
        parts.append(end)

    # Safety
    if N is not None:
        assert(len(parts) == N)
    if keyword is not None:
        assert(parts[0] == keyword)

    return parts

# Checks if s starts with given keyword
# Abstracted to remove duplication with formatting
def checkKeyword(expression, keyword):
    k = len(keyword)
    if expression[:k+1] == (keyword + " "):
        return True
    if expression[:k+1] == (keyword + "("):
        return True
    return False

def checkKeywordList(expression, keyword_list):
    for keyword in keyword_list:
        if checkKeyword(expression, keyword):
            return True
    return False

# Format assignations in let, struct and fun definitions
# Can set custom spacers and assigners
def formatAssignations(s, spacer, delimiter, eval = False):
    assignations = unwrapNlet(s, None)
    
    # Reformat assignations
    formatted_assignations = []
    for assignation in assignations:
        [var, expr] = unwrapNlet(assignation[1:-1], 2)
        if eval:
            expr = f"({convertExpression(expr)})"
        formatted_assignations.append(f"{var}{spacer}{expr}")
    assignations_string = delimiter.join(formatted_assignations)
    return assignations_string

# Convert struct from .snek format to .bet format
# This function should not be recursive
def convertStruct(struct):
    [_, name, sig] = unwrapNlet(struct, 3, "struct")
    assignations_string = formatAssignations(sig[1:-1], "::", ", ")
    return f"struct {name} ({assignations_string});{NEWLINE}{NEWLINE}"
    
# Convert function from .snek format to .bet format
# This function should not be recursive
def convertFunction(fun):
    [_, name, sig, ret, body] = unwrapNlet(fun, 5, "fun")
    assignations_string = formatAssignations(sig[1:-1], "::", ", ")
    body_expr = convertExpression(body)
    return f"fun {name} ({assignations_string})::{ret} {OPEN_BRACE}{NEWLINE}{body_expr}{NEWLINE}{CLOSE_BRACE};{NEWLINE}{NEWLINE}"
    
# Convert expression from .snek format to .bet format
# This function should be recursive
def convertExpression(expression):
    # Safe unwrap for parsing
    expression = expression.strip(" ")
    if expression == "":
        return ""
    if expression[0] == "(":
        expression = expression[1:-1]
    if expression == "":
        return ""
    
    # Cased transpilation, can be uncased with a generic unwrapNlet but this was easier to implement and debug
    if checkKeyword(expression, "alloc"):
        [_, ty] = unwrapNlet(expression, 2, "alloc")
        return f"new {ty}"
    elif checkKeyword(expression, "null"):
        [_, ty] = unwrapNlet(expression, 2, "null")
        return f"null {ty}"
    elif checkKeywordList(expression, UNOPS):
        [op1, expr] = unwrapNlet(expression, 2)
        value = convertExpression(expr)
        return f"{op1} ({value})"
    elif checkKeyword(expression, "set!"):
        [_, var, expr] = unwrapNlet(expression, 3, "set!")
        value = convertExpression(expr)
        return f"{var} := ({value})" 
    elif checkKeyword(expression, "update"):
        [_, obj, field, expr] = unwrapNlet(expression, 4, "update")
        var = convertExpression(obj)
        value = convertExpression(expr)
        return f"{var}.{field} := ({value})"        
    elif checkKeyword(expression, "lookup"):
        [_, obj, field] = unwrapNlet(expression, 3, "lookup")
        var = convertExpression(obj)
        return f"{var}.{field}"
    elif checkKeyword(expression, "let"):
        [_, binds, expr] = unwrapNlet(expression, 3, "let")
        assignations = formatAssignations(binds[1:-1], ":=", ",\n", True)
        value = convertExpression(expr)
        return f"let ({NEWLINE}{assignations}{NEWLINE}) {OPEN_BRACE} {NEWLINE}{value} {NEWLINE}{CLOSE_BRACE}"
    elif checkKeyword(expression, "block"):
        parts = unwrapNlet(expression, None, "block")
        assignations_string = ";\n".join([convertExpression(part) for part in parts[1:]])
        return assignations_string
    elif checkKeyword(expression, "if"):
        [_, cond, if_true, if_false] = unwrapNlet(expression, 4, "if")
        cond_expr = convertExpression(cond)    
        true_expr = convertExpression(if_true)    
        false_expr = convertExpression(if_false)
        return f"if ({cond_expr}) {OPEN_BRACE} {NEWLINE}{true_expr} {NEWLINE}{CLOSE_BRACE} else {OPEN_BRACE} {NEWLINE}{false_expr} {NEWLINE}{CLOSE_BRACE}"
    elif checkKeyword(expression, "repeat-until"):
        [_, body, cond] = unwrapNlet(expression, 3)
        body_expr = convertExpression(body)    
        cond_expr = convertExpression(cond)
        return f"do {OPEN_BRACE} {NEWLINE}{body_expr} {NEWLINE}{CLOSE_BRACE} until ( {NEWLINE}{cond_expr} {NEWLINE})"
    elif checkKeyword(expression, "="): # Handled seperately as the = becomes ==
        [_, expr1, expr2] = unwrapNlet(expression, 3)
        value1 = convertExpression(expr1)    
        value2 = convertExpression(expr2)
        return f"({value1}) == ({value2})"
    elif checkKeywordList(expression, BINOPS):
        [op2, expr1, expr2] = unwrapNlet(expression, 3)
        value1 = convertExpression(expr1)    
        value2 = convertExpression(expr2)
        return f"({value1}) {op2} ({value2})"

    # Check for call
    parts = unwrapNlet(expression, None)
    if len(parts) > 1:
        fun_name = parts[0]
        args = [convertExpression(part) for part in parts[1:]]
        all_args = ", ".join(args)
        return f"{fun_name}({all_args})"

    return f"{expression}"

# Convert program from .snek format to .bet format
# This should not be recursive
def convertProgram(program):
    chunks = unwrapNlet(program, None)
    chunks = [chunk.replace("\n", "").replace("\t", "").replace("    ", "").strip() for chunk in chunks]
    chunks = list(filter(lambda x: len(x), chunks))

    converted_program = ""
    for chunk in chunks:
        if chunk[1:8] == "struct ":
            converted_program += convertStruct(chunk[1:-1])
        elif chunk[1:5] == "fun ":
            converted_program += convertFunction(chunk[1:-1])
        else:
            converted_program += convertExpression(chunk)
            assert(chunk == chunks[-1])
    
    return converted_program

# Remove comments from snek file by discarding text between ; and \n
# Simulate mini FSM
def removeComments(snek_program):
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

# Adds indentation to program
# Uses double tracer method
def addIndentation(converted_program):
    formatted_program = []
    indent = "  "
    excess_paren = 0
    excess_brace = 0
    cur_line = ""
    i = 0
    for c in converted_program:
        # Safety
        assert(excess_brace >= 0)
        assert(excess_paren >= 0)
        i += 1
        if c == "\n":
            formatted_program.append(cur_line)
            if i < len(converted_program) and (converted_program[i] == "}" or converted_program[i] == ")"): 
                cur_line = indent * (excess_paren + excess_brace - 1)
            else:
                cur_line = indent * (excess_paren + excess_brace)    
        else:
            cur_line += c
            if c == "{":
                excess_brace += 1
            elif c == "}":
                excess_brace -= 1
            elif c == "(":
                excess_paren += 1
            elif c == ")":
                excess_paren -= 1
    formatted_program.append(cur_line)

    return "\n".join(formatted_program)

# transpiles file from .snek to .bet, creates new .bet file
def transpile(dirname, filename):
    old_file = open(f"./tests/{dirname}/{filename}.snek", 'r')
    snek_program = old_file.read()
    
    uncommented_program = removeComments(snek_program)
    converted_program = convertProgram(uncommented_program)
    formatted_program = addIndentation(converted_program)

    new_file = open(f"./tests/{dirname}-bet/{filename}.bet", "w")
    new_file.write(formatted_program)
    new_file.close()

# Wraps transpile to transpile everything in a directory
def transpileDirectory(dirname):
    for filename in os.listdir(f"./tests/{dirname}"):
        filename = filename[:-5] # drop filetype
        try:
            transpile(dirname, filename)
        except Exception as e:
            print(f"Transpilation failed for {dirname}/{filename}.snek")

# Main Loop
# Transpile everything
for directory in TARGET_DIRECTORIES:
    transpileDirectory(directory)