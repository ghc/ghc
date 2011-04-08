#!/bin/sh
cd `dirname $0`

runhaskell -hide-all-packages -cpp \
    -packagecontainers \
    -packagearray \
    -packagebase \
    -packageghc \
    -packagexhtml \
    -packageghc-paths \
    -packageHUnit \
    -i../../dist/build/ \
    -i../../src/ \
    -optP-include -optP../../dist/build/autogen/cabal_macros.h \
    parsetests.hs
