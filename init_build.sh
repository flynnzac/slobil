#!/bin/sh

libtoolize && aclocal && autoconf && automake --add-missing

git submodule update
cd pushmac && sh init_build.sh && cd ..
