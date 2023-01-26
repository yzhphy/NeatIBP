#!/bin/sh

echo 1..4

INPUT_MATRIX=$srcdir/Matrix/t1.sms

# test 1 : reading/writing a matrix in triplet form
OUT=$srcdir/Output/io.1
EXPECTED=$srcdir/Expected/io.1

./io 1 < $INPUT_MATRIX > $OUT

if diff -w $OUT $EXPECTED >/dev/null ; then
  echo 'ok 1 - reading and writing a matrix'
else
  echo 'not ok 1 - reading and writing a matrix'
fi

# test 2 : reading a matrix in triplet form, writing it in CSR
OUT=$srcdir/Output/io.2
EXPECTED=$srcdir/Expected/io.2

./io 2 < $INPUT_MATRIX > $OUT

if diff -w $OUT $EXPECTED >/dev/null ; then
  echo 'ok 2 - reading a matrix, converting it to CSR and writing it'
else
  echo 'not ok 2 -  reading a matrix, converting it to CSR and writing it'
fi

# test 3 : GBLA old format input
INPUT_MATRIX=$srcdir/Matrix/small.gbla.old
OUT=$srcdir/Output/io.3
EXPECTED=$srcdir/Expected/io.3

cat $INPUT_MATRIX | ./gbla_in_old | cut -d ' ' -f 1,2 > $OUT
if diff -w $OUT $EXPECTED >/dev/null ; then
  echo 'ok 3 - reading a matrix in old GBLA format, and writing it in SMS format'
else
  echo 'not ok 3 -  reading a matrix in old GBLA format, and writing it in SMS format'
fi

# test 4 : GBLA new format input
INPUT_MATRIX=$srcdir/Matrix/small.gbla.new
OUT=$srcdir/Output/io.4
EXPECTED=$srcdir/Expected/io.3

cat $INPUT_MATRIX | ./gbla_in_new | cut -d ' ' -f 1,2 > $OUT
if diff -w $OUT $EXPECTED >/dev/null ; then
  echo 'ok 4 - reading a matrix in new GBLA format, and writing it in SMS format'
else
  echo 'not ok 4 -  reading a matrix in new GBLA format, and writing it in SMS format'
fi
