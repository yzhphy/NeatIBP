#!/bin/sh

echo 1..1


# test 1 : reading/writing a matrix in triplet form
INPUT_MATRIX=$srcdir/Matrix/singular.sms
OUT=$srcdir/Output/submatrix.1
EXPECTED=$srcdir/Expected/submatrix.1

./submatrix 1 < $INPUT_MATRIX > $OUT

if diff -w $OUT $EXPECTED >/dev/null ; then
    echo 'ok 1 - range submatrix'
else
  echo 'not ok 1 - range submatrix'
fi