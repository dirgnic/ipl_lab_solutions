[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/QN25Qu45)
# Installation

Run the tests with

```
stack test
```

You should get some failures, but after you have completed this homework,
all tests should pass.

# Homework

We consider a language with arithmetic expressions, variables, and a binding
`Let` construct. We want to translate this language to LLVM IR.

Complete the code generation in file `src/Hw02.hs`.

Add one more test.

## Optional: Add a mutliplication expression

Add another variant `Mul Exp Exp` to the type of expressions.

## Optional: Deal with shadowing

There is one tricky test case that is commented out: a variable shadows another
variable. There are different strategies for dealing with this.

