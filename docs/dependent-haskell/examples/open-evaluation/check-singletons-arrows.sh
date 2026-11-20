#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if (( $# < 2 || $# > 3 )); then
    echo "Usage: $0 FORK_GHC LIBDIR [STOCK_GHC]" >&2
    exit 2
fi
compiler=$(realpath "$1")
lib=$(realpath "$2")
stock=
if (( $# == 3 )); then stock=$(realpath "$3"); fi
src=$(cd "$(dirname "$0")" && pwd)
full="$src/../../../../testsuite/tests/dependent/should_compile/GADTSingletons.hs"
flags=(-v0 -O0 -package-env - -fdiagnostics-color=never -fomit-interface-pragmas
       -fno-write-if-simplified-core -dcore-lint -dstg-lint)
ghc=("$compiler" "-B$lib" -XNoExperimentalDependentTypes -fprint-explicit-matchabilities "${flags[@]}")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

provider=SingletonsArrowProvider
client=SingletonsArrowClient

reject() {
    local code=$1 diagnostic=$2
    shift 2
    if "$@" >reject.log 2>&1 || ! grep -Fq "$code" reject.log ||
       ! grep -Fq "$diagnostic" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

# Change the equations in a fresh directory for each compiler invocation.
for variant in bare synonym-M split polymorphic; do
    mkdir -p "$out/$variant/client"
    cd "$out/$variant"
    cp "$src/$provider.hs" "$full" .
    if [[ $variant == bare || $variant == synonym-M || $variant == polymorphic ]]; then
        sed -i.bak '/^type instance Promote (a %M -> b)/d' "$provider.hs"
    fi
    if [[ $variant == bare ]]; then
        sed -i.bak -e '/^.*LANGUAGE Modifiers/d' -e '/^import GHC.Matchability/d' \
            -e 's/TyFun a b %M -> Type/TyFun a b -> Type/' "$provider.hs"
        reject '[GHC-83865]' 'data TyCon1' "${ghc[@]}" -c "$provider.hs"
        reject '[GHC-83865]' 'data (~>@#@$)' "${ghc[@]}" -c GADTSingletons.hs
        if [[ -n $stock ]]; then
            "$stock" "${flags[@]}" -c GADTSingletons.hs
            "$stock" "${flags[@]}" -c "$provider.hs"
            cp "$provider.hi" "$src/$client.hs" client/
            (cd client && "$stock" "${flags[@]}" -c "$client.hs")
        fi
        continue
    fi

    sed -i.bak -e '1i\
{-# LANGUAGE Modifiers #-}
' -e '/^import Data.Kind$/a\
import GHC.Matchability (M, Matchability)
' -e 's/TyFun a b -> Type/TyFun a b %M -> Type/' GADTSingletons.hs
    case $variant in
        split)
            sed -i.bak '/^type instance Promote (a -> b)/a\
type instance Promote (a %M -> b) = PromoteX a ~> PromoteX b
' GADTSingletons.hs ;;
        polymorphic)
            for file in "$provider.hs" GADTSingletons.hs; do
                sed -i.bak \
                    -e 's/import GHC.Matchability (M)$/import GHC.Matchability (M, Matchability)/' \
                    -e 's/Promote (a -> b)/Promote (a %(m :: Matchability) -> b)/' "$file"
            done ;;
    esac

    "${ghc[@]}" -c "$provider.hs"
    cp "$provider.hi" "$src/$client.hs" client/
    if [[ $variant == synonym-M ]]; then
        reject '[GHC-83865]' 'type instance PromoteX (Either a)' "${ghc[@]}" -c GADTSingletons.hs
        (cd client && reject '[GHC-83865]' 'type instance PromoteX Maybe' "${ghc[@]}" -c "$client.hs")
    else
        "${ghc[@]}" -c GADTSingletons.hs
        (cd client && "${ghc[@]}" -c "$client.hs")
    fi

    # A client can give the M case its own result. The bare row must not overlap it.
    if [[ $variant == synonym-M || $variant == polymorphic ]]; then
        cd client
        cat >Separate.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, Modifiers, TypeFamilies, TypeOperators #-}
module Separate where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M)
import SingletonsArrowProvider

type instance Promote (a %M -> b) = Ordering

constructor :: Promote (Type %M -> Type) :~: Ordering
constructor = Refl
HS
        if [[ $variant == synonym-M ]]; then
            "${ghc[@]}" -c Separate.hs
        else
            reject '[GHC-34447]' 'Conflicting family instance declarations' "${ghc[@]}" -c Separate.hs
        fi
    fi
done
