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

## New syntax

- `type` can now include `<name>`, which indicates a struct.
- These structs are declared via the `<defn>` type, which is expanded to include a new keyword: `struct`.
    - Each `struct` has a name and **at least one** field, each field containing a name and a type.
    - Field names must be unique within a struct.
    - Field types can include any type, including `int`, `bool`, mutually-recursive structs, or any other struct.
    - Structs can be declared in any order; all struct names are parsed, then type-checking occurs.
- Instances of structs, accessible via pointer, are created with `alloc`. Fields of new structs are initialized to 0 for integers, `null` for structs, and `false` for bools.
- Fields of structs can be updated and evaluated with `lookup` and `update`, both of which evaluate to the value in the struct field (the new value for `update`).
- `(null <name>)` creates a pointer of type `<name>` which has value `null`.
- `e1 = e2` for structs checks for pointer equality.
- The `print` keyword, when used on a struct type, points the following format:
    - Non-null pointer:

    ```
    struct <name>
            <field_1_name> : <runtime_value>
            ...
            <field_n_name> : <runtime_value>
    ```

    - Null pointer:
    ```
    null pointer to struct <name>
    ```

## Diagram of Heap-Allocation

## Required Tests

### `input/simple_examples.snek`

```bash
% ./tests/input/simple_examples.run   
1
2
2
```

### `input/points.snek`

```
% ./tests/input/points.run
struct point (4297069072)
        x: int = 4
        y: int = 16
struct point (4297069088)
        x: int = 6
        y: int = 18
struct point (4297069104)
        x: int = 8
        y: int = 20
null pointer to struct point
```

This program demonstrates how printing structs works for us. When you print a pointer to a non-null struct, it loops through the fields in order, and prints the runtime values. It also points the address of the struct.

One limitation of our program is that we didn't have time to implement is saving type information; we just print everything as an int; for pointer fields within a struct, you can still compare the address.

When you call print on a struct pointer with value 0x0, ie, null, it displays that the struct you printed is null, as well as the type of the pointer.

### `input/bst.snek`

```
% ./tests/input/bst.run
struct bst (4308189680) ; root
        val: int = 4
        left: bst = 0
        right: bst = 0
null pointer to struct bst ; root's left child
null pointer to struct bst ; root's right child
struct bst (4308189680) ; root
        val: int = 4
        left: bst = 4308189704
        right: bst = 0
struct bst (4308189704) ; root's left child
        val: int = 3
        left: bst = 0
        right: bst = 0
null pointer to struct bst ; root's right child
struct bst (4308189680) ; root
        val: int = 4
        left: bst = 4308189704
        right: bst = 0
struct bst (4308189704) ; root's left child
        val: int = 3
        left: bst = 4308189728
        right: bst = 0
null pointer to struct bst ; root's right child
struct bst (4308189680) ; root
        val: int = 4
        left: bst = 4308189704
        right: bst = 4308189752
struct bst (4308189704) ; root's left child
        val: int = 3
        left: bst = 4308189728
        right: bst = 0
struct bst (4308189752) ; root's right child
        val: int = 5
        left: bst = 0
        right: bst = 0
```

In this example, we start by allocating a tree with value `4` and no children. 

Then for each of: `3, 2, 5` (in that order), we insert the new number into the BST and then print the root (which contains 4).

As the trace shows, initally the root has `null` for left and right. Once we insert `3`, since that's smaller than 4 it is inserted as the left child of the root, which is now non null. When we insert `2`, since it's smaller than 4 it belongs to the left subtree, and is inserted as the left child of `3`. When we insert `5`, which is larger than 4, it goes into the right subtree, which is why in the final printout the root's right child is non-null.

### `input/error-alloc.snek`

```
% ./tests/input/error-alloc.run
Runtime error: out of space
```

This program calls `alloc` in a `repeat-until-false` loop (which would run forever), except it encounters a runtime exception when our bump-allocator runs out of room to allocate the next struct.

This error happens at runtime, as `lookup` / `update` check for null-pointer dereference.

### `input/error-read.snek`

```
% make tests/input/error-read.run
cargo run -- tests/input/error-read.snek tests/input/error-read.s
   Compiling cobra v0.1.0 (/Users/elijahbaraw/Desktop/private/17363/hw/hw4-cobra/17363-Cobra)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.68s
     Running `target/debug/cobra tests/input/error-read.snek tests/input/error-read.s`
thread 'main' panicked at src/typecheck.rs:638:25:
Invalid: Lookup nonexistent field value in struct ll
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
make: *** [tests/input/error-read.s] Error 101
```

This test demonstrates what happens when a program attempts to access a nonexistent field; it throws a **compile time** error, which identified the struct name, and the field name which isn't in that struct. This error happens at compile time, during typechecking (after parsing, before compiling).

From the declaration of `ll`, it has no field `value` (the correct field is `val`): `(struct ll ((val int) (next ll)))`.

### `input/error3.snek`

```
% ./tests/input/error3.run
Runtime error: null dereference
```

In this test, a null pointer is dereferenced at runtime, which generates a runtime exception: `Runtime error: null dereference`.

## Comparison to Real Programing Languages

## List of Resources

**Designing a Bump Allocator:**
https://cohost.org/eniko/post/171803-basic-memory-allocat 

**Storing and Accessing a .bss Array:**
https://stackoverflow.com/questions/34058101/referencing-the-contents-of-a-memory-location-x86-addressing-modes 
https://www.reddit.com/r/asm/comments/16tdhvh/data_vs_bss_for_uninitialized_data/ 

**Pretty Printing and String Methods:**
https://stackoverflow.com/questions/70096134/assembly-store-a-string-in-register
https://docs.rs/windows-win/latest/windows_win/sys/struct.CStr.html