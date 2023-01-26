#!/bin/sh

echo 1..5

#########################################
INPUT_MATRIX=$srcdir/Matrix/small.sms
./dense_lu 1 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/medium.sms
./dense_lu 2 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/singular.sms
./dense_lu 3 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_h.sms
./dense_lu 4 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_l.sms
./dense_lu 5 < $INPUT_MATRIX