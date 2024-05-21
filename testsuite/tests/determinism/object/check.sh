#!/bin/sh

# set -e

if test -z "$1"
then
    echo "Usage: ./check.sh <ghc>"
    exit 1
fi

UNAME=$(uname)
COMPILER="$1"

# On darwin we need to pass -m to objdump on Mach-O files
if test $UNAME == "Darwin"
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

# $1 = objects
# $2 = extra flags
compareHis() {
    for o in $2
    do
        echo $1 --show-iface $o
        echo "--------------------------------------------------------------------------------"
        # Compare the object dumps except for the first line which prints the file path
        $1 --show-iface Cabal-3.12.0.0/hiout1/$o > dump1
        $1 --show-iface Cabal-3.12.0.0/hiout2/$o > dump2
        diff -C3 dump1 dump2 && echo "OK-hi"
        echo "--------------------------------------------------------------------------------"
    done
}

if diff -r Cabal-3.12.0.0/hiout1 Cabal-3.12.0.0/hiout2
then
    echo "OK-hi"
else
    echo "--------------------------------------------------------------------------------"
    echo "Comparing all objects (1. headers, 2. disassembly). Stopping at first failure..."
    echo "--------------------------------------------------------------------------------"


    pushd Cabal-3.12.0.0/hiout1 >/dev/null
    OBJS=$(find . -type f)
    popd >/dev/null

    compareHis "$COMPILER" "$OBJS"

    exit 1

fi

# Big fast check
if diff -r Cabal-3.12.0.0/out1 Cabal-3.12.0.0/out2
then
    echo "OK-obj"
else
    echo "--------------------------------------------------------------------------------"
    echo "Comparing all objects (1. headers, 2. disassembly). Stopping at first failure..."
    echo "--------------------------------------------------------------------------------"


    pushd Cabal-3.12.0.0/out1 >/dev/null
    OBJS=$(find . -type f)
    popd >/dev/null

    compareObjs "$OBJS" "--all-headers"

    compareObjs "$OBJS" "--disassemble-all"
    exit 1

fi


