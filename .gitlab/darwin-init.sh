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
    cabal_tarball="https://www.haskell.org/cabal/release/cabal-install-2.2.0.0/cabal-install-2.2.0.0-x86_64-apple-darwin-sierra.tar.gz"
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

