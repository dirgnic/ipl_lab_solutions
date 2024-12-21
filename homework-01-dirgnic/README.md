[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/tOcg-zSA)
# Installation

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

It should print `false` or `true`.


# Homework

We consider a language of boolean expressions with boolean literals,
`and`, `or`, `not` and `xor`.

Complete file `src/Main.hs` to implement LLVM code generation for `Exp`.
You will have to find suitable constructors of the
[data type of LLVM constants](https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST-Constant.html#t:Constant).
You will not find a constructor for unary `not`, so you will have to
desugar `not` to something else. Also consider the
[LLVM documentation on constants](http://llvm.org/docs/LangRef.html#constants)
and [constant expressions](http://llvm.org/docs/LangRef.html#constant-expressions).

Try your solution on different examples by changing the definition of
`myExpression`.

Improve your Haskell workflow by trying different editors and plugins.

