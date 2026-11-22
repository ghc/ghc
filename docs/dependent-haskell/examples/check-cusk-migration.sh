#!/usr/bin/env bash
# Compare a legacy kind signature and its standalone replacement.
set -euo pipefail
export LC_ALL=C.UTF-8
if [[ $# != 2 ]]; then
    echo "Usage: bash $0 GHC LIBDIR" >&2
    exit 2
fi

ghc=("$(realpath "$1")" "-B$(realpath "$2")" -v0 -O0 -package-env -
     -fforce-recomp -fdiagnostics-color=never -i -i.)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

for mode in CUSKs NoCUSKs StandaloneKindSignatures; do
    mkdir -p "$work/$mode"/{provider,client}
    cd "$work/$mode/provider"
    cat >Provider.hs <<HS
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExplicitForAll, GADTs, PolyKinds, $mode #-}
module Provider (Test) where
import Data.Kind (Type)
HS
    if [[ $mode == StandaloneKindSignatures ]]; then
        cat >>Provider.hs <<'HS'
type Test :: forall x. x -> x -> x -> Type
data Test a b c where
HS
    else
        echo 'data Test (a :: x) (b :: x) :: x -> Type where' >>Provider.hs
    fi
    echo '    K :: Test Int Bool Double' >>Provider.hs

    if [[ $mode == NoCUSKs ]]; then
        if "${ghc[@]}" -c Provider.hs >compile.log 2>&1; then
            echo "Expected GHC to reject the legacy declaration with NoCUSKs" >&2
            exit 1
        fi
        grep -Fq 'GHC-25897' compile.log || { cat compile.log >&2; exit 1; }
        continue
    fi

    "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core -c Provider.hs
    cp Provider.hi ../client/
    cd ../client
    cat >Client.hs <<'HS'
{-# LANGUAGE Haskell2010, DataKinds #-}
module Client where
import Data.Proxy (Proxy(..))
import Provider (Test)

use :: Proxy (Test 'True 'False 'True)
use = Proxy
HS
    "${ghc[@]}" -c Client.hs
done
echo 'The client compiled with both kind signatures. NoCUSKs rejected the legacy declaration.'
