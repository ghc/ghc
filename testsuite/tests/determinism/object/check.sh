#!/bin/sh

# set -e

if test -z "$1"
then
    echo "Usage: ./check.sh <HostOS> # example ./check.sh darwin"
    exit 1
fi

# On darwin we need to pass -m to objdump on Mach-O files
if test $1 == "darwin"
then
    OBJDUMP="objdump -m"
else
    OBJDUMP="objdump"
fi

# Guarantee object files were written

S1=`find Cabal-3.12.0.0/out1 -name "*.o" | wc -l`
S2=`find Cabal-3.12.0.0/out2 -name "*.o" | wc -l`

test $S1 > 0 || (echo "no files generated" && false)
test $S1 == $S2

# $1 = objects
# $2 = extra flags
compareObjs() {
    for o in $1
    do
        echo $OBJDUMP $2 $o
        echo "--------------------------------------------------------------------------------"
        # Compare the object dumps except for the first line which prints the file path
        $OBJDUMP $2 Cabal-3.12.0.0/out1/$o | tail -n+2 > dump1
        $OBJDUMP $2 Cabal-3.12.0.0/out2/$o | tail -n+2 > dump2
        diff dump1 dump2 && echo "OK"
        echo "--------------------------------------------------------------------------------"
    done
}

# Big fast check
if diff -r Cabal-3.12.0.0/out1 Cabal-3.12.0.0/out2
then
    echo "OK"
else
    echo "--------------------------------------------------------------------------------"
    echo "Comparing all objects (1. headers, 2. disassembly). Stopping at first failure..."
    echo "--------------------------------------------------------------------------------"


    pushd Cabal-3.12.0.0/out1 >/dev/null
    OBJS=$(find . -type f)
    popd >/dev/null

    compareObjs "$OBJS" "--all-headers"

    compareObjs "$OBJS" "--disassemble-all"

fi


