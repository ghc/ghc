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
    -i../dist/build/ \
    -i../src/ \
    parsetests.hs
