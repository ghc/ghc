#! /bin/sh

make clean boot
make -k |& tee log-normal
make clean boot
make -k EXTRA_HC_OPTS=-threaded |& tee log-threaded
make clean boot
make -k EXTRA_HC_OPTS=-smp |& tee log-smp-N1
make -k EXTRA_HC_OPTS=-smp EXTRA_RUNTEST_OPTS='+RTS -N2 -RTS' |& tee log-smp-N2
make -k EXTRA_HC_OPTS=-smp EXTRA_RUNTEST_OPTS='+RTS -N8 -RTS' |& tee log-smp-N8
make -k EXTRA_HC_OPTS=-smp EXTRA_RUNTEST_OPTS='+RTS -N16 -RTS' |& tee log-smp-N16
