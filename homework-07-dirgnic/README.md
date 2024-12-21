[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/_XF-5K4d)
# Homework

We depart from the functional style and add an imperative feature: allocation.
We can allocate on the stack or on the heap.

https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl07.html

Complete `checkStm` and `lowerStm`.

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


