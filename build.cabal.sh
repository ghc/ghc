#!/usr/bin/env bash

set -euo pipefail

absoltueRoot="$(dirname "$(readlink -f "$0")")"
cd "$absoltueRoot"

# Initialize sandbox if necessary
if ! ( cabal sandbox hc-pkg list 2>&1 > /dev/null ); then
    cabal sandbox init
    cabal install                   \
        --dependencies-only         \
        --disable-library-profiling \
        --disable-shared
fi

cabal run ghc-shake --             \
    --lint                         \
    --directory "$absoltueRoot/.." \
    --colour                       \
    "$@"
