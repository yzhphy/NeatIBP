#!/bin/bash
cd SpaSM_for_NeatIBP
autoreconf -i
automake --add-missing
automake
./configure
make
make check
make install
