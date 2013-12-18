# Core Language description

This file describes the constructs that form the core language.

## Basics

1. Identifiers need to start with a lowercase letter, and can contain only letters and numbers.
2. Keywords so far: `if`, `then`, `else`, `and`, `or`, `not`, `let`, `in`. Will add more as we move along
3. The language is strongly typed. Type annotations are done using two colons: `4 :: Int`.
4. Type inference is used. Type annotation should be not needed, if all goes well.
5. The core part of the language does for now not allow mutations. We will see if there is a need for it.

## Numbers

1. There are two kinds of number types, `Int` and `Dbl` for integers and doubles respectively.
2. Integers are infinite precision integers.
3. Doubles are standard 32-bit floating numbers.
4. Standard arithmetic operators can act on either type, and also convert `Int`s to `Dbl`s if necessary. That is the only automatic conversion.
5. Evaluation on the terms in operators happens left-to-right as usual.
6. TODO: Add list of built-in functions

## Booleans

1. The type `Bool` is for booleans.
2. The literals `true` and `false` represent truth and falsehood respectively.
3. Operators `not`, `and`, `or` are provided with the above keyword.
4. The binary operators short-circuit: Their second argument is evaluated only if necessary.

## String

1. Type `Str` for representing Unicode strings.
2. TODO: Not implemented yet.
3. TODO: Should add regexp support.
4. TODO: Should add standard string manipulation.

## Vector

1. The `Vec a` vector type is for a finite list of homogeneous elements, of type `a`.
2. Any type can be used for the contents, including `Vec` itself. This produces arrays.
3. Vector expressions are written as follows: `[e1,e2,...,en]`.
4. Evaluation of the vector contents happens in a left-to-right direction as usual.
5. TODO: vectors should have a default `length` method.
6. TODO: Other standard vector operations, at the very least map/filter/fold.

## Tuples

1. Tuples are finite fixed length sequences of inhomogeneous elements.
2. Written in the form `(e1,e2,...)`.
3. Evaluation proceeds from left to right.
4. The type of a tuple is exactly specified by the types of the expressions in it.
5. Internally, tuples are treated just as records, with indices starting from 1. In particular their access patterns are those of records, for example `t.1` accesses the first element in the tuple.
6. TODO: Make sure `Unit` type is implemented via empty tuple.

## Record

1. A record is like a dictionary in other languages: A series of "key-value" pairs.
2. Literal notation: `{a: 2.2, b: {c: 1, d: true}}`
3. The type of the record is determined by the list of keys and the types of the corresponding values. The ordering of the keys does not matter, so the records `{a:2, b:1}` and `{b:3, a:1}` have the same type.
4. Accessing the fields of a record happens via a dot notation: `rec.a`.

## Symbols

1. TODO: Should implement them.
2. Use a single quote followed by alphanumeric to represent them.
3. Or should we encourage enumerated types instead?

## Functions

1. Functions are first class citizens, whose type is an "arrow" type `t1 -> t2`.
2. Anonymous function definition is done as follows: `(\x -> x+x)`.
3. Functions in curried form are allowed: `(\x y -> x+y)` automatically gets converted to `(\x -> (\y -> x+y))`.
4. Function application uses s-expression: `(f x)`. Multiple curried arguments are allowed: `(f x y)` is syntactic sugar for `((f x) y)`.
5. Function evaluation proceeds by first evaluating the expression in the function side, then proceeds to evaluate the remaining arguments, then attempts to apply the function.
6. Functions operate under lexical scope.

## Let expressions
1. Let expressions have the form `let p1=e1 p2=e2 p3=e3 in body`, where the multiple assignments are just syntactic sugar for a chain of let expressions; in other words let acts as let-star in ML.
2. The p's are pattern expressions. More on them later.
3. The body is evaluated under the bindings described by the `p=e` expressions.

## Pattern Expressions

1. TODO: Describe them.
2. Should be used in case expressions and in let expressions.

