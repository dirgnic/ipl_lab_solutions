[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/M0HDp-X-)
# Homework

Finally, we want to write a parser for our language. Come up with your own concrete
syntax and complete the parser in module `Parsing`. Any flavour of syntax is fine, e.g.
Lispy, C-like, Scala-esque, Rust-style ...

Change the existing golden test in `test-data/parsing/` and add at least five more.

Consult the Megaparse tutorial: https://markkarpov.com/tutorial/megaparsec.html

Optional: Make your parser indentation-sensitive.

https://markkarpov.com/tutorial/megaparsec.html#indentationsensitive-parsing

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
stack exec pli-hw05 -- test-data/parsing/ten.input
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


