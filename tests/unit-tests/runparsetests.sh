#!/bin/sh
cd `dirname $0`

runhaskell \
    -i../../src \
    -i../../dist/build/autogen \
    -i../../dist/build/haddock/haddock-tmp/ \
    -packageghc \
    -optP-include \
    -optP../../dist/build/autogen/cabal_macros.h \
    -XCPP \
    -XDeriveDataTypeable \
    -XScopedTypeVariables \
    -XMagicHash \
    parsetests.hs
