#!/bin/sh

echo 1..5

#########################################
INPUT_MATRIX=$srcdir/Matrix/medium.sms
./transpose 1 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/upper_trapeze.sms
./transpose 2 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/lower_trapeze.sms
./transpose 3 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/u1.sms
./transpose 4 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/l1.sms
./transpose 5 < $INPUT_MATRIX
