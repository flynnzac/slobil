# ONBU

ONBU is a programming language based on a named-list data type called a registry and instructions to modify the contents of that registry.  Data is stored at named locations in the registry called registers.  The language has a highly regular syntax where each statement is an instruction to modify the current registry or move between different registries. ONBU is an interpreted language and much of its design is focused on being easy to use interactively.  The language contains instructions for writing the output of an interactive session out to disk enabling interactive creation of programs.

ONBU is short by one letter for On Bus, which is where I wrote the language: on the bus to work. The first bug-filled version in something like its current form was written in roughly Fall/Winter 2019.

For a manual see: 

http://www.zflynn.com/onbu/onbu.html 


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
$ ./configure ...
$ make
```

The `configure` script takes all the usual options (`--prefix`, for example).

The program is built in the current directory. To start the interpreter, type:
```
$ ./onbu
```

To install the program, type:
```
$ sudo make install
```

# Command-line Switches

ONBU accepts the following command-line switches:

- `-l FILE` executes the file before starting the interpreter.
- `-s FILE` executes the file after any loading done by an `-l` option and exits after executing the script.
- `-c FILE` like `-s` but treats the first line of the FILE as a comment (allows you to use the sha-bang syntax `#!/usr/bin/onbu -c` to execute the file as a script).
- `-n` do not start the interpreter. Exit after any loading.
- `-m` do not print out information unless explicitly using a `print` instruction.
- `-d` do not save code as entered. This option prevents using commands to write out the code you have entered interactively, but might give a small performance boost. Implied by `-s`.

# Emacs

ONBU has an Emacs mode! It has syntax highlighting, indentation, and you can spawn ONBU sessions and send code from the buffer to the interpreter. The emacs mode is in the `emacs` folder. It requires `isend-mode` which you can get from MELPA.




