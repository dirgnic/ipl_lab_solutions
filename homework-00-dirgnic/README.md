[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/WlviRTrS)
# Installation on Ubuntu

## Install [LLVM 15](https://apt.llvm.org/)

Add the following repository to your sources:

```
deb http://apt.llvm.org/noble/ llvm-toolchain-noble-15 main
```

Install llvm 15 for development:

```
sudo apt install llvm-15-dev
```

## Install [`ghcup`](https://www.haskell.org/ghcup/)

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## Install [`stack`](https://docs.haskellstack.org/en/stable/README/)

```
ghcup install stack
```

## Install this project

Clone this project:

```
git clone https://github.com/se-tuebingen-pli/hw00-MYNAME.git
```

Move into the newly created directory:

```
cd hw00-MYNAME
```

Build the project:

```
stack build
```

Run the project:

```
stack run
```

There should be a file `myModule.o` in your current directory.

# Homework

Follow the installation instructions.

Start learning the basics of Haskell by doing any Haskell tutorial.

Note that with this setup in order to start `ghci` you need to enter the command `stack ghci`.

## Optional: linking by hand

You can link `myModule.o` with a minimal runtime system found in `rts/main.c` to get an executable:

```
gcc rts/main.c myModule.o
```

And run it:

```
./a.out
```

It should print `5`.

## Optional: look at the generated machine code

You can inspect the file `myModule.o` with

```
objdump -d myModule.o
```

This should print

```

myModule.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <myMain>:
   0:	b8 05 00 00 00       	mov    $0x5,%eax
   5:	c3                   	retq
```

