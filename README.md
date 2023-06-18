# SLOBIL

SLOBIL is a programming language based on two types of data: Slots and Objects.  Objects are both the environment in which code is executed, a `struct`-like datatype, and an Object in the sense of Object-Oriented Programming. Slots are locations within an Objects where Data can be stored. The language has a highly regular syntax where each statement is an instruction to modify the current object or move between different objects. SLOBIL is designed to be easy to use interactively and to build interactive programs.  Because SLOBIL's syntax itself is command-like, many "programs" can simply be libraries of instructions that use SLOBIL's REPL for user interaction. 

SLOBIL stands for SLot-and-Object Based Interactive Language.

Some features:

- Prototype-based Object-Oriented Programming with automatic propagation of changes to the parent Object to all children.
- Instructions (i.e. functions or methods) can be executed in user-defined contexts.  There is not a fixed set of arguments that must be provided to an instruction.  Any element in the instruction can be redefined when it is called except a Literal.
- Language is useful for interactive use.  Built-in methods to log sessions, save Objects to disk, etc.
- Strong typing with a Tcl-ish syntax

For a manual see: 

http://www.zflynn.com/slobil/slobil.html 


# Installation

I have tested that the interpreter works on GNU/Linux and under MSYS
on Windows. The required dependencies are:

- GNU Readline 
- libunistring
- libgc

The current build system uses GNU Autotools. If that is installed,
then to build and install, do:

```
$ ./init_build.sh
$ ./configure 
$ make
```

The `configure` script takes all the usual options (`--prefix`, for example).

The program is built in the current directory. To start the interpreter, type:
```
$ ./slobil
```

To install the program, type:
```
$ sudo make install
```

# Command-line Switches

SLOBIL accepts the following command-line switches:

- `-l FILE` executes the file before starting the interpreter.
- `-s FILE` executes the file after any loading done by an `-l` option and exits after executing the script.
- `-c FILE` like `-s` but treats the first line of the FILE as a comment (allows you to use the sha-bang syntax `#!/usr/bin/slobil -c` to execute the file as a script).
- `-n` do not start the interpreter. Exit after any loading.
- `-m` do not print out information unless explicitly using a `print` instruction.
- `-d` do not save code as entered. This option prevents using commands to write out the code you have entered interactively, but might give a small performance boost. Implied by `-s`.

# Emacs

SLOBIL has an Emacs mode! It has syntax highlighting, indentation, and you can spawn SLOBIL sessions and send code from the buffer to the interpreter. The emacs mode is in the `emacs` folder. It requires `isend-mode` which you can get from MELPA.




