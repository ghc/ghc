#!/usr/bin/env bash

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]:-$0}"; )" &> /dev/null && pwd 2> /dev/null; )";

cd "$SCRIPT_DIR/test-jsem"

export GHC_JSEM_PATH="/dougrulz"
export GHC_JSEM_COUNT="12"
export GHC_SHIM_PATH="$(readlink -e ../_build/stage1/bin/ghc)"

rm -fr dist-newstyle store

mkdir store
./test-jsem.hs --store-dir="$(readlink -e store)" build -j \
    --only-dependencies \
    --with-ghc="$(readlink -e ghc-shim.hs)" \
    --with-ghc-pkg="$(dirname "$GHC_SHIM_PATH")/ghc-pkg" \
    --ghc-options="-jsem /dougrulz"
