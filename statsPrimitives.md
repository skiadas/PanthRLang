A description of the stats primitives, i.e. the math part of the core language

## Primitive structures

- Numbers: Two kinds of numbers, infinite precision Integer's and Double's
- Strings: Implemented as primitives. Literals enclosed in quotes
- Booleans: True/False
- Factor: An enum

## Compound structures

- Vectors: Containing values of specific type. Fixed length for now
- Records: a set of named values. Names must be alphanums
- Functions: Allow curried argument syntax and recursion
- Tuples: 
- References: Perhaps not needed?
- Patterns?

## Constructs

- if-then-else, and/or
- Addition, multiplication, mod, div, other elementary operations on integers and doubles
- anonymous function definition \x y -> ...
- function application
- define constructs for creating "top-scope" things
- Vector notation [..,..]. Allow for ranges a..b ?
- Tuple notation (..,..,..)
- Record notation { a: ..., b: ... }
- Find a convenient way for denoting map/filter/fold
- let-in
