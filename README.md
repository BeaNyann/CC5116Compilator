# CDI-reference
Starter code for compilers homework

## Requirements & Setup

See the [detailed description](https://users.dcc.uchile.cl/~etanter/CC5116-2020/#(part._materials)) on the page of the course.

## Organization of the repository

The organization of the repository is designed for the development of your compiler. 

- `compiler/`: the compiler, defined as a dune library 
(using a library allows us to play with our code in a REPL, see below)
- `bin/`: top-level executables for the compiler and tests 
- `tests/`: test files for the compiler 

- `bbctester/`: a library for supporting compiler test files

- `dune-workspace`, `dune-project`: root configuration for the dune package manager
- `Makefile`: shortcuts to build and test

Dune will build everything inside the `_build/` directory.

## Organization inside of compiler

- `asm`: assembler instructions with its arguments options 
- `compile`: the compiler itself 
- `envs` : variables, functions and C functions enviroment systems
- `errors` : functions to check type errors in runtime
- `expr`: diferents type of expresions that exists
- `funcalls`: functions to follow the 64-bit calling convention and C function call handling
- `interp`: the interpreter of the language
- `parse`: the parser of the language
- `prims`: the compilation of unary and binary primitives
- `rtsys`: call our functions and converts boolean and integers types 
- `tag`: tagging system of the AST 


## Decisions made

- Make the lets multibinding without transforming them into nested lets
- Abstract the AST with the types Prim1 to the unary operations, Prim2 to the binary operations, Comp to the comparison operations (this abstraction was done because the difference in the assembler code of these operations were small)
- Using the A-Normal Form to the binary operations
- Using tagging for the if conditions. Then incorporate that tagging system to all the nodes of the AST to help in debugging tasks
- Using cmp for the comparison operations
- Using the x86-64 calling convention to both C functions and functions in our source language
- Changing the syntax of calling foreign functions from (defsys fun_name arg1_type arg2_type ... argn_type -> return_type) to 
(defsys fun_name (arg1_type arg2_type ... argn_type) -> return_type) to reduce the complexity of identifying the group of argument's types
- Using a desuggar function to change syntax sugar, adding source expressions and core expressions.
- Using lambdas to make first class functions
- Free variables of lambdas are stored using the stack
- Tuple index starts at 0

## Features implemented

- The unary operations Add1, Sub1 and Not
- Let-bindings with single and multiple bindings
- The binary operations Plus (+), Times (*), Minus (-), DividedBy (/), And, Or, Less (<) and Equal (=) 
- Evaluation Shortcuts to the binary operations And and Or
- If conditionals
- Dynamic handling of type errors
- Compiling external function call of print
- First order functions 
- Foreign functions interface (the ability to call a C function in our program)
- Tuples with the operations (tup v1 .. vn) and (get e k) to get a value from the tuple
- Tuples are able to be empty
- Mutation for tuples with the operation (set e k v)
- Pattern matching for tuples using (tup v1 ..vn) inside of a let
- Records, tuples build with a constructor and with accesible fields
- Records are able to have no fields
- Operation seq to run a sequence of actions
- Lambdas to make first class functions
- Garbage collector implemented

## Fixes made (entrega 4)
- lambdas wiuth 0 arguments working
- Calling C functions inside lambdas
- print of lambdas
- functions returning other functions
- Calling C functions witrh 5 or less, 6 and 7 or more arguments
- fixed lambdas using free variables

## Fixes made (entrega 5)
- proper stages of garbage collection implemented, now it can handle structures with pointer to the heap
- fixed heap scanning and repeated code, now it can copy values in every frame
