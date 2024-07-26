#!/bin/sh

if test -z "$1"
then
    echo "Usage: ./check-standalone.sh <ghc>"
    exit 1
fi

rm -rf objs1 objs2
cabal get Cabal-3.12.0.0
cabal build --enable-profiling -w "$1" --ghc-options="-fforce-recomp -j12 -ddump-to-file -fobject-determinism -dumpdir=dumpout1 -ddump-simpl -ddump-stg-final -ddump-cmm" --ghc-options=-odir=out1 --ghc-options=-hidir=hiout1 Cabal
# cabal build -w $1 --ghc-options="-fforce-recomp -j4" --ghc-options=-odir=out1 Cabal
cabal build --enable-profiling -w "$1" --ghc-options="-fforce-recomp -j12 -ddump-to-file -dinitial-unique=16777215 -dunique-increment=-1 -fobject-determinism -dumpdir=dumpout2 -ddump-simpl -ddump-stg-final -ddump-cmm" --ghc-options=-odir=out2 --ghc-options=-hidir=hiout2 Cabal
# cabal build -w $1 --ghc-options="-fforce-recomp -j4" --ghc-options=-odir=out2 Cabal
./check.sh "$1"
