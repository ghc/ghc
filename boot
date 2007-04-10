#!/bin/sh

set -e

autoreconf

for lib in libraries/*; do
    if test -e $lib/configure.ac
    then
        cd $lib
        autoreconf
        cd ../..
    fi
done

