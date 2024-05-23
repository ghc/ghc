#!/bin/sh

if test -z "$1"
then
    echo "Usage: ./check-standalone.sh <ghc>"
    exit 1
fi

rm -rf objs1 objs2
cabal get Cabal-3.12.0.0
cabal build -w $1 --ghc-options="-fforce-recomp -j4" --ghc-options=-odir=out1 Cabal
cabal build -w $1 --ghc-options="-fforce-recomp -j4" --ghc-options=-odir=out2 Cabal
./check.sh darwin
