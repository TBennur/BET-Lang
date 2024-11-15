## Concrete Syntax

Please note: the braces, `[]`, are currently not used for anything; they just serve to show what the `*` and `+` operators are being applied to, without being confused for parentheses

```c
<prog> := <defn>* <expr>

<defn> := 
  | fun <name>()::<type> <block>; // zero params
  | fun <name>(<name>::<type>[, <name>::<type>]*)::<type> <block>; // 1+ params
  | struct <name> (<name>::<type>[, <name>::<type>]*); // structs have 1+ fields

<expr> :=
  | <number>  // naturals (incl 0)
  | ~<number> // this is how we make negative numbers
  | true
  | false
  | input
  | <identifier>
  | let (<binding>[, <binding>]*) <block>
  | <op1> <expr>
  | <wrapped_expr> <op2> <wrapped_expr>
  | <name> := <expr>
  | if (<expr>) <block> else <block>
  | {<expr>[; <expr>]*} // block
  | do <block> until (<expr>)
  | <name> (<expr>*) // function call
  | null <name>
  | new <name> // alloc
  | <expr>.<name> // lookup
  | <expr>.<name> := <wrapped_expr> // update
  | <wrapped_expr>

<wrapped_expr> := 
  // don't need to be wrapped as they're a single thing
  | <number>
  | ~<number> // since the ~ is a "sticky" operator
  | true
  | false
  | input
  | <identifier>

  // already wrapped in curly braces
  | block

  // need to wrap, specifically when used in binops
  | (let (<binding> [, <binding>]*) <block>)
  | (<op1> <expr>)
  | (<wrapped_expr> <op2> <wrapped_expr>)
  | (<name> := <expr>)
  | (if (<expr>) <block> else <block>)
  | (do <block> until (<expr>))
  | (<name> (<expr>*))
  | (null <name>)
  | (new <name>)
  | (<expr>.<name>)
  | (<expr>.<name> := <wrapped_expr>)
  | (<wrapped_expr>) // when parsing, this is simply unwrapped; extra parens have no meaning

<op1> := add1 | sub1 | print
<op2> := + | - | * | < | > | >= | <= | ==

<type> := int | bool | <name>

<binding> := <identifier> := <expr>
```