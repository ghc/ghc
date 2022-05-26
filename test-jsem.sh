#!/usr/bin/env bash

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]:-$0}"; )" &> /dev/null && pwd 2> /dev/null; )";

cd "$SCRIPT_DIR/test-jsem"
export GHC_JSEM="/dougrulz"
rm -fr dist-newstyle store
mkdir store
./test-jsem.hs --store-dir="$(readlink -e store)" build -j \
    --only-dependencies \
    --with-ghc="$(readlink -e ../_build/stage1/bin/ghc)" \
    --ghc-options="-jsem /dougrulz"
