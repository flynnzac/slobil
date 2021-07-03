#!/bin/bash

libtoolize && aclocal && autoconf && automake --add-missing

git submodule init
git submodule update --checkout

cd pushmac && sh init_build.sh && cd ..

