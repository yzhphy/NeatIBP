#!/bin/sh

echo 1..10

#########################################
INPUT_MATRIX=$srcdir/Matrix/small.sms
./pluq 1 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/medium.sms
./pluq 2 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/singular.sms
./pluq 3 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_h.sms
./pluq 4 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_l.sms
./pluq 5 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/small.sms
./permuted_pluq 6 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/medium.sms
./permuted_pluq 7 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/singular.sms
./permuted_pluq 8 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_h.sms
./permuted_pluq 9 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_l.sms
./permuted_pluq 10 < $INPUT_MATRIX
