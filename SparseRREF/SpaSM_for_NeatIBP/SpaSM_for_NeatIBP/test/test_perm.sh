#!/bin/sh

echo 1..2

#########################################
INPUT_MATRIX=$srcdir/Matrix/small.sms
./mat_perm 1 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/upper_trapeze.sms
./mat_perm 2 < $INPUT_MATRIX
