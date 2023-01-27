#!/bin/bash
git clone https://github.com/cbouilla/spasm.git
cp modules.c spasm/bench/modules.c
cp Makefile.am spasm/bench/Makefile.am
cd spasm
autoreconf -i
automake --add-missing
automake
./configure
make
make check
make install
