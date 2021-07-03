#!/bin/sh

libtoolize && aclocal && autoconf && automake --add-missing

sh pushmac/init_build.sh
