[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/h5f9XgtC)
# Homework

We add functions to our language. Complete the type checker by tracking the
types of top-level function definitions. Add at least two test cases.

Optional: Add a parser.

# Testing

Run the tests with

```
stack test
```

# Execution

Build the project with

```
stack build
```

Run the executable and supply the path of an input file with

```
stack run -- myModule.input
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


