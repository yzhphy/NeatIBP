#!/bin/sh

echo 1..4

#########################################
./dm 1 < $srcdir/Matrix/dm.sms
./dm 2 < $srcdir/Matrix/dm2.sms
./dm 3 < $srcdir/Matrix/BIOMD0000000424.int.mpl.sms 
./dm 4 < $srcdir/Matrix/BIOMD0000000525.int.mpl.sms