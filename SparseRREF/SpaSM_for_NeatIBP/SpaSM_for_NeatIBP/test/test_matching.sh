#!/bin/sh

echo 1..4

#########################################

./matching 1 < $srcdir/Matrix/t1.sms
./matching 2 < $srcdir/Matrix/dm.sms
./matching 3 < $srcdir/Matrix/dm2.sms
./matching 4 < $srcdir/Matrix/BIOMD0000000424.int.mpl.sms
