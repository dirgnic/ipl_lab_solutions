[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/PHGWeoE9)
# Homework

We consider a language with boolean and arithmetic expressions, let bindings
and variables. We have already implemented lowering to ANF and generation of LLVM
code from ANF.

## Implement type checking

Complete the function `infer` in file `src/TypeChecking.hs`. Add at least
one more test case for type checking.

## Add two more language primitives

Add integer subtraction (`Sub`) and boolean not (`Not`) to the souce language `Exp`
and drag them through the entire compilation pipeline.

## Optional: Add equality

Add an equality expression that works on integers and booleans as long as both
arguments have the same type.

# Testing

Run the tests with

```
stack test
```

# Execution

Enter the REPL with

```
stack repl
```

From within the REPL, run the main function with

```
main
```

This will print an error, but after you have completed this homework,
you should find a file `myModule.o` in your current directory.

Link it with a minimal runtime system with

```
gcc rts/main.c myModule.o
```

and run it with

```
./a.out
```

It should print some number.


