#!/bin/bash

set -e
set -u

THREAD_LIST="1000 2000 4000 6000 8000 10000 15000 20000 25000 30000 40000"

NEW_BIN="./new-thread-delay"
OLD_BIN="./old-thread-delay"

NEW_DAT="new.dat"
OLD_DAT="old.dat"

make data_clean

make clean
make thread-delay
mv thread-delay $NEW_BIN

make clean
make thread-delay USE_GHC_IO_MANAGER=1
mv thread-delay $OLD_BIN

# Format: threads,time(s)

echo -n "Benchmarking old I/O manager..."
for n in $THREAD_LIST
do
    echo -en "$n\t" >> $OLD_DAT
    (time -p $OLD_BIN -n $n) 2>&1 | awk '$1 == "user" {print $2}' | cat >> $OLD_DAT
done
echo "done"

echo -n "Benchmarking new I/O manager..."
for n in $THREAD_LIST
do
    echo -en "$n\t" >> $NEW_DAT
    (time -p $NEW_BIN -n $n) 2>&1 | awk '$1 == "user" {print $2}' | cat >> $NEW_DAT
done
echo "done"

gnuplot thread-delay.gnuplot
