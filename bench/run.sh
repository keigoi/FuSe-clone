#!/bin/sh
MYDIR=`dirname $0`
OUTFILE=$MYDIR/result_$1$2.csv

echo "$1" > $OUTFILE
for i in `seq 0 999`;
do
  $MYDIR/$1.opt $2 >>$OUTFILE
done
