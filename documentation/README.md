## Concrete Syntax

```
<prog> := <defn>* <expr>
<defn> := 
  | (fun <name> ((<name> <type>)*) <type> <expr>)
  | (struct <name> ((<name> <type>)+) )
<expr> :=
  | <number>
  | true
  | false
  | input
  | <identifier>
  | (let (<binding>+) <expr>)
  | (<op1> <expr>)
  | (<op2> <expr> <expr>)
  | (set! <name> <expr>)
  | (if <expr> <expr> <expr>)
  | (block <expr>+)
  | (repeat-until <expr> <expr>)
  | (<name> <expr>*)
  | (null <name>)
  | (alloc <name>)
  | (lookup <expr> <name>)
  | (update <expr> <name> <expr>)

<op1> := add1 | sub1 | print
<op2> := + | - | * | < | > | >= | <= | =

<type> := int | bool | <name>

<binding> := (<identifier> <expr>)
```


## Add Concrete Syntax for Structs:

- DONE: Add in struct parser
- DONE: Update program definition
- DONE: Update function definition
- DONE: Update type definition
- DONE: Update expression definition
- DONE: We need to null keyword (pointer to any type)
- DONE: We will include alloc, update and lookup commands
- DONE: Add struct and null as reserved

## Add Parsing:

- DONE: Allow recursive and mutually recursive structures
- DONE: All defined structures are implicitly wrapped as pointers
- DONE: Functions and structures can be defined in any order
- DONE: Parse structures then functions (as we must know structure fields for valid functions)

## Add Type Checking:

- DONE: Structures are either well-typed or point to null
- DONE: Prevent setting to a different type
- DONE: Allow let rebinding to a different type

## Add Compiling:

- Struct definitions don't cause anything to compile, but lookup, alloc and update do
- Add call to malloc to add space
  - plus runtime check of malloc returning NULL
- Update print external function
- Add runtime null dereference checking (add to standard error)
- Allow heap data to be read
- Allow heap data to be manipulated
- Add error handling for failed malloc call

## Open Questions:

- Should we print an entire structure or just the contents?
- Should printing be formatted?


