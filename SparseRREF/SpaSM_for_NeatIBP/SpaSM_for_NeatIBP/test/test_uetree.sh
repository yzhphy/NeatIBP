#!/bin/sh

echo 1..5

#########################################
./uetree 1 < $srcdir/Matrix/scc.sms
./uetree 2 < $srcdir/Matrix/scc2.sms
./uetree 3 < $srcdir/Matrix/scc3.sms
./uetree 4 < $srcdir/Matrix/mat364.sms
./uetree 5 < $srcdir/Matrix/tree_test.sms