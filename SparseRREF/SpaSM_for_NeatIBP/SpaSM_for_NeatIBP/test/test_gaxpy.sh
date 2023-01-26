#!/bin/sh

echo 1..4

#########################################
INPUT_MATRIX=$srcdir/Matrix/m1.sms
OUT=$srcdir/Output/gaxpy.1
EXPECTED=$srcdir/Expected/gaxpy.1
./gaxpy < $INPUT_MATRIX > $OUT

if diff -w $OUT $EXPECTED >/dev/null ; then
  echo 'ok 1 - matrix-vector product'
else
  echo 'not ok 1 - matrix-vector product'
fi

#####################################################"
INPUT_MATRIX=$srcdir/Matrix/m1.sms
./sparse_vect_matrix_prod 2 < $INPUT_MATRIX

#####################################################"
INPUT_MATRIX=$srcdir/Matrix/medium.sms
./sparse_vect_matrix_prod 3 < $INPUT_MATRIX

#####################################################"
INPUT_MATRIX=$srcdir/Matrix/rectangular_l.sms
./sparse_vect_matrix_prod 4 < $INPUT_MATRIX
