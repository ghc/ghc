#!/bin/bash

set -e

toolchain=`pwd`/toolchain
PATH="$toolchain/bin:$PATH"

if [ -d "`pwd`/cabal-cache" ]; then
    cp -Rf cabal-cache $HOME/.cabal
fi

if [ ! -e $toolchain/bin/ghc ]; then
    mkdir -p tmp
    cd tmp
    curl https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-x86_64-apple-darwin.tar.xz | tar -xJ
    cd ghc-$GHC_VERSION
    ./configure --prefix=$toolchain
    make install
    cd ../..
    rm -Rf tmp
fi

if [ ! -e $toolchain/bin/cabal ]; then
    cabal_tarball="https://downloads.haskell.org/~cabal/cabal-install-latest/cabal-install-2.4.1.0-x86_64-apple-darwin-sierra.tar.xz"
    curl $cabal_tarball | tar -xz
    mv cabal $toolchain/bin
fi

if [ ! -e $toolchain/bin/happy ]; then
    cabal update
    cabal new-install happy --symlink-bindir=$toolchain/bin
fi

if [ ! -e $toolchain/bin/alex ]; then
    cabal update
    cabal new-install alex --symlink-bindir=$toolchain/bin
fi

