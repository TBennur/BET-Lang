## Concrete Syntax

Please note: the braces, `[]`, are currently not used for anything; they just serve to show what the `*` and `+` operators are being applied to, without being confused for parentheses

```c
<prog> := <defn>* <expr>
<defn> := 
  | fun <name>()::<type> <block>; // zero params
  | fun <name>(<name>::<type> [, <name>::<type>]*)::<type> <block>; // 1+ params
  | struct <name> (<name>::<type> [, <name>::<type>]*); // structs have 1+ fields
<expr> :=
  | <number>
  | true
  | false
  | input
  | <identifier>
  | let (<binding> [, <binding>]*) <block>
  | <op1> <expr>
  | <expr> <op2> <expr>
  | <name> := <expr>
  | if (<expr>) <block> <block>
  | {<expr> [, <expr>]*} // block
  | do <block> until (<expr>)
  | <name> (<expr>*) // function call
  | null <name>
  | new <name> // alloc
  | <expr>.<name> // lookup
  | <expr>.<name> := <expr> // update

<op1> := add1 | sub1 | print
<op2> := + | - | * | < | > | >= | <= | =

<type> := int | bool | <name>

<binding> := <identifier> := <expr>
```