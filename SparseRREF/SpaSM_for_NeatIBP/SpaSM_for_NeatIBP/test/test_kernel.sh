#!/bin/sh

echo 1..8

#########################################
INPUT_MATRIX=$srcdir/Matrix/singular.sms
./kernel 1 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/lower_trapeze.sms
./kernel 2 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/upper_trapeze.sms
./kernel 3 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/dm.sms
./kernel 4 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/dm2.sms
./kernel 5 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/void.sms
./kernel 6 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/BIOMD0000000525.int.mpl.sms
./kernel 7 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/BIOMD0000000424.int.mpl.sms
./kernel 8 < $INPUT_MATRIX
