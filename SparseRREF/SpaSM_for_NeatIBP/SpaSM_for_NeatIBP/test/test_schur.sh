#!/bin/sh

echo 1..16

#########################################
INPUT_MATRIX=$srcdir/Matrix/singular.sms
./schur 1 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/lower_trapeze.sms
./schur 3 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/upper_trapeze.sms
./schur 5 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/dm.sms
./schur 7 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/dm2.sms
./schur 9 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/void.sms
./schur 11 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/BIOMD0000000525.int.mpl.sms
./schur 13 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/BIOMD0000000424.int.mpl.sms
./schur 15 < $INPUT_MATRIX
