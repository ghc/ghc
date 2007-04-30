#!/bin/sh

set -e

chmod +x rts/gmp/configure
chmod +x darcs-all

./darcs-all ${1+"$@"} get

autoreconf

for lib in libraries/*; do
    if test -e $lib/configure.ac
    then
        cd $lib
        autoreconf
        cd ../..
    fi
done

