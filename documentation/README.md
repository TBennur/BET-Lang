## Add Concrete Syntax for Structs:
- Add in struct parser
- Update program definition
- Update function definition
- Update type definition
- Update expression definition
- We need to null keyword (pointer to any type)
- We will include alloc, update and lookup commands
- Add struct and null as reserved
## Add Parsing:
- Allow recursive and mutually recursive structures
- All defined structures are implicitly wrapped as pointers
- Functions and structures can be defined in any order
- Parse structures then functions (as we must know structure fields for valid functions)
## Add Type Checking:
- Structures are either well-typed or point to null
- Prevent setting to a different type
- Allow let rebinding to a different type
## Add Compiling:
- Add call to malloc to add space
- Update print external function
- Add runtime null dereference checking (add to standard error)
- Allow heap data to be read
- Allow heap data to be manipulated
- Add error handling for failed malloc call
## Open Questions:
- Should we print an entire structure or just the contents?
- Should printing be formatted?


