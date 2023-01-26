#!/bin/sh

echo 1..4

#########################################
INPUT_MATRIX=$srcdir/Matrix/cc.sms
./cc 1 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/BIOMD0000000525.int.mpl.sms
./cc 2 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/empty.sms
./cc 3 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/void.sms
./cc 4 < $INPUT_MATRIX
