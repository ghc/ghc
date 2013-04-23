#!/bin/bash

echo "PROGRAM="$1
for i in 48 32 16 8 4 2 1
do
	echo "NumProcessors="$i
	for j in {0..5}
	do
		time $1 +RTS -N$i -RTS 5000 > /dev/null
	done
done
