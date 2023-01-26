#!/bin/sh

echo 1..4

#########################################
./scc 1 < $srcdir/Matrix/scc.sms
./scc 2 < $srcdir/Matrix/scc2.sms
./scc 3 < $srcdir/Matrix/scc3.sms
./scc 4 < $srcdir/Matrix/mat364.sms