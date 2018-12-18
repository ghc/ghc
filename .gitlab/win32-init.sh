#!/bin/bash

set -e

toolchain=`pwd`/toolchain
PATH="$toolchain/bin:/mingw64/bin:$PATH"

if [ -d "`pwd`/cabal-cache" ]; then
    cp -Rf cabal-cache $APPDATA/cabal
fi

if [ ! -e $toolchain/bin/ghc ]; then
    curl https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-x86_64-unknown-mingw32.tar.xz | tar -xJ
    mv ghc-$GHC_VERSION toolchain
fi

if [ ! -e $toolchain/bin/cabal ]; then
    curl https://www.haskell.org/cabal/release/cabal-install-2.2.0.0/cabal-install-2.2.0.0-i386-unknown-mingw32.zip > /tmp/cabal.zip
    unzip /tmp/cabal.zip
    mv cabal.exe $toolchain/bin
fi

if [ ! -e $toolchain/bin/happy ]; then
    cabal update
    cabal install happy
    cp $APPDATA/cabal/bin/happy $toolchain/bin
fi

if [ ! -e $toolchain/bin/alex ]; then
    cabal update
    cabal install alex
    cp $APPDATA/cabal/bin/alex $toolchain/bin
fi

