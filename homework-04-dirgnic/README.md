[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/XOukMNxK)
# Homework

We want to compile a language with `let` statements, control flow in the form
of `if then else` and loops that iterate for a fixed number of times.

## Find Predecessors

Complete the function `findPredecessors` in module `CodeGeneration`.

## Add a `For` (loop) statement

Add a statement `For Exp Var Exp Stm` that executes a statement for a fixed
number of iterations. The first expression is the number of iterations. The
variable will be bound to the second expression in the first execution of the
statement and then be rebound to the result of the previous execution in
subsequent executions. The overall result is the result of the statement after
the last execution. For example

```
For (Num 5) "x" (Num 4) (Res (Add (Var "x") (Num 3)))
```

should yield the number `19`.

## Optional: Add a `Brk` (break) statement

Add a `Brk Exp` statement that breaks out of the closest loop with a value. This
might require you to add a second continuation parameter to `lowerStm` that
tracks the exit of the closest loop. If there was no loop it should end the
program with the value. For example:

```
For (Num 5) "i" (Num 1) (
  Ite (Sma (Var "i") (Num 3)) (
    Res (Add (Var "i") (Num 1))) (
    Brk (Num 25)))
```

should yield the number `25`.

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

