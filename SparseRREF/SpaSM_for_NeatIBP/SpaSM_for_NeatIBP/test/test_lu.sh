#!/bin/sh

echo 1..10

#########################################
INPUT_MATRIX=$srcdir/Matrix/small.sms
./lu 1 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/medium.sms
./lu 2 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/singular.sms
./lu 3 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_h.sms
./lu 4 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_l.sms
./lu 5 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/small.sms
./permuted_lu 6 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/medium.sms
./permuted_lu 7 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/singular.sms
./permuted_lu 8 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_h.sms
./permuted_lu 9 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_l.sms
./permuted_lu 10 < $INPUT_MATRIX