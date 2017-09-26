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

init bench N
init bench Z
init bench_monad 
init bench_so

init bench NP
init bench ZP
init bench_monad P

init bench_ipc N
init bench_ipc Z
init bench_ipc_monad

init bench2 N
init bench2_so S
init bench2 NA
init bench2_so A
init bench_ipc_monad

init bench_ipc_so

for i in `seq 0 999`;
do
single bench N
single bench Z
single bench_monad 
single bench_so

single bench NP
single bench ZP
single bench_monad P

single bench_ipc N
single bench_ipc Z
single bench_ipc_monad

single bench2 N
single bench2_so S
single bench2 NA
single bench2_so A

single bench_ipc_so
#./intelpowergadget
echo $i
done




