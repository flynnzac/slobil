# ARBEL

ARBEL is a programming language based on a named-list data type called a registry and instructions to modify the contents of that registry.  Data is stored at named locations in the registry called registers.  The language has a highly regular syntax where each statement is an instruction to modify the current registry or move between different registries. ARBEL is an interpreted language and much of its design is focused on being easy to use interactively.  The language contains instructions for writing the output of an interactive session out to disk enabling interactive creation of programs.

For a manual see: http://www.zflynn.com/arbel/arbel.html

__NEWS: The basic syntax of ARBEL is now finalized and won't change going forward.__


# Installation

I have tested that the interpreter works on GNU/Linux and under MSYS on Windows. GNU Readline is a required depedency. Other than the standard GNU C Library, nothing else is required to build.

To build, do:

```
make
```

from the source directory. The make process is extremely simple (a one-line compilation command) so nothing else should be required.  If you do not have a Make program, you can just copy the compilation command from the Makefile and execute it. 

The program is built in the current directory. To start the interpreter, type:
```
./arbel
```

# Command-line Switches

ARBEL accepts the following command-line switches:

- `-l FILE` executes the file before starting the interpreter.
- `-s FILE` executes the file after any loading done by an `-l` option and exits after executing the script.
- `-n` do not start the interpreter. Exit after any loading.
- `-m` do not print out information unless explicitly using a `print` instruction.
- `d` do not save code as entered. This option prevents using commands to write out the code you have entered interactivel, but might give a small performance boost. Implied by `-s`.

# Emacs

ARBEL has an Emacs mode! It has syntax highlighting, indentation, and you can spawn ARBEL sessions and send code from the buffer to the interpreter. The emacs mode is in the `emacs` folder. It requires `isend-mode` which you can get from MELPA.




