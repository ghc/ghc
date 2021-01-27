#!/bin/bash

set -e
set -u

THREAD_LIST="1000 2000 4000 6000 8000 10000 15000 20000 25000 30000 40000"

EVENTS_BIN="./events"
THREADS_BIN="./threads"

EVENTS_DAT="events.dat"
THREADS_DAT="threads.dat"

make data_clean
make clean

make timers
mv timers $EVENTS_BIN

make thread-delay
mv thread-delay $THREADS_BIN

# Format: threads,time(s)

echo -n "Benchmarking events..."
for n in $THREAD_LIST
do
    echo -en "$n\t" >> $EVENTS_DAT
    (time -p $EVENTS_BIN -n $n) 2>&1 | awk '$1 == "user" {print $2}' | cat >> $EVENTS_DAT
done
echo "done"

echo -n "Benchmarking threads..."
for n in $THREAD_LIST
do
    echo -en "$n\t" >> $THREADS_DAT
    (time -p $THREADS_BIN -n $n) 2>&1 | awk '$1 == "user" {print $2}' | cat >> $THREADS_DAT
done
echo "done"

gnuplot timers.gnuplot
