#!/bin/sh

echo 1..13

#########################################
INPUT_MATRIX=$srcdir/Matrix/l1.sms
./dense_lsolve 1 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/lower_trapeze.sms
./dense_lsolve 2 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/u1.sms
./dense_usolve 3 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/upper_trapeze.sms
./dense_usolve 4 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/u1.sms
./sparse_usolve 5 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/upper_trapeze.sms
./sparse_usolve 6 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/l1.sms
./sparse_lsolve 7 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/lower_trapeze.sms
./sparse_lsolve 8 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/u1.sms
./sparse_utsolve 9 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/upper_trapeze.sms
./sparse_utsolve 10 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/lower_trapeze.sms
./sparse_lsolve_big 11 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/singular2.sms
./sparse_permuted_lsolve 12 < $INPUT_MATRIX

#########################################
INPUT_MATRIX=$srcdir/Matrix/singular3.sms
./sparse_permuted_lsolve 13 < $INPUT_MATRIX