#!/bin/sh

## Assumes you have already make installed wob so that libwob is on your path and that the
## library is installed in /usr/local/lib.  Modify path if installed elsewhere.

gcc -c -fPIC -o link.o link.c
gcc -shared -Wl,-rpath -Wl,/usr/local/lib -o link.so link.o -lwob
