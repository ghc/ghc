#!/bin/bash

set -e

toolchain=`pwd`/toolchain
PATH="$toolchain/bin:/mingw64/bin:$PATH"

if [ -d "`pwd`/cabal-cache" ]; then
    cp -Rf cabal-cache $APPDATA/cabal
fi

if [ ! -e $toolchain/bin/ghc ]; then
    case $MSYSTEM in
      MINGW32)
        triple="i386-unknown-mingw32"
        ;;
      MINGW64)
        triple="x86_64-unknown-mingw32"
        ;;
      *)
        echo "win32-init: Unknown MSYSTEM $MSYSTEM"
        exit 1
        ;;
    esac
    curl https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-$triple.tar.xz | tar -xJ
    mv ghc-$GHC_VERSION toolchain
fi

if [ ! -e $toolchain/bin/cabal ]; then
    url="https://downloads.haskell.org/~cabal/cabal-install-2.4.1.0/cabal-install-2.4.1.0-x86_64-unknown-mingw32.zip"
    curl $url > /tmp/cabal.zip
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

if [[ -z ${INTEGER_LIBRARY:-} ]]; then INTEGER_LIBRARY=integer-gmp; fi
cat > mk/build.mk <<EOF
include mk/flavours/${BUILD_FLAVOUR}.mk

V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_SPHINX_HTML=YES
BUILD_SPHINX_PDF=NO
BeConservative=YES
INTEGER_LIBRARY=$INTEGER_LIBRARY
GhcLibHcOpts+=-haddock
EOF

echo "================================================="
echo "Build.mk:"
echo ""
cat mk/build.mk
echo "================================================="
