#!/bin/sh
MYDIR=`dirname $0`

function init() {
  OUTFILE=$MYDIR/result_$1$2.csv
  echo "$1$2" > $OUTFILE
}

function single() {
  OUTFILE=$MYDIR/result_$1$2.csv
  $MYDIR/$1.opt $2 >>$OUTFILE
}

init bench0 Z
init bench0_monad 
init bench Z
init bench_monad 

for i in `seq 0 999`;
do
single bench0 Z
single bench0_monad 
single bench Z
single bench_monad 

echo $i
done




