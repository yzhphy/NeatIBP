#!/bin/sh

echo 1..4

#########################################
INPUT_MATRIX=$srcdir/Matrix/g1.sms
OUT=$srcdir/Output/reach.1
EXPECTED=$srcdir/Expected/reach.1
./reach 0 < $INPUT_MATRIX > $OUT

if diff -w $OUT $EXPECTED >/dev/null ; then
  echo 'ok 1 - DFS'
else
  echo 'not ok 1 - DFS'
fi

#########################################
OUT=$srcdir/Output/reach.2
EXPECTED=$srcdir/Expected/reach.2
./reach 1 < $INPUT_MATRIX > $OUT

if diff -w $OUT $EXPECTED >/dev/null ; then
  echo 'ok 2 - DFS'
else
  echo 'not ok 2 - DFS'
fi

#########################################
INPUT_MATRIX=$srcdir/Matrix/upper_trapeze.sms
OUT=$srcdir/Output/reach.3
EXPECTED=$srcdir/Expected/reach.3
./reach 5 < $INPUT_MATRIX > $OUT

if diff -w $OUT $EXPECTED >/dev/null ; then
  echo 'ok 3 - DFS (m > n)'
else
  echo 'not ok 3 - DFS (m > n)'
fi

OUT=$srcdir/Output/reach.4
EXPECTED=$srcdir/Expected/reach.4
./reach 24 < $INPUT_MATRIX > $OUT

if diff -w $OUT $EXPECTED >/dev/null ; then
  echo 'ok 4 - DFS (m > n)'
else
  echo 'not ok 4 - DFS (m > n)'
fi
