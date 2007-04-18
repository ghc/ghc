#!/bin/sh

set -e

for d in `cat libraries/core-packages`
do
    if test ! -d libraries/$d
    then
        echo "Looks like you're missing libraries/$d,"
        echo "maybe you haven't done 'sh darcs-all get'?"
        exit 1
    fi
done

autoreconf

for lib in libraries/*; do
    if test -e $lib/configure.ac
    then
        cd $lib
        autoreconf
        cd ../..
    fi
done

