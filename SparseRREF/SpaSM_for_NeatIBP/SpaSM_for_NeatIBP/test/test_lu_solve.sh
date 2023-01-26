#!/bin/sh

echo 1..5

#########################################
INPUT_MATRIX=$srcdir/Matrix/small.sms
./lu_solve 1 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/medium.sms
./lu_solve 2 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/singular.sms
./lu_solve 3 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_h.sms
./lu_solve 4 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/rectangular_l.sms
./lu_solve 5 < $INPUT_MATRIX
