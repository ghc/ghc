#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

if (( $# != 2 )); then
    echo "Usage: $0 GHC LIBDIR" >&2
    exit 2
fi
src=$(cd "$(dirname "$0")" && pwd)
ghc=("$(realpath "$1")" "-B$(realpath "$2")" -i -i. -v0 -package-env - -dcore-lint -dstg-lint)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

for opt in O0 O2; do
    root="$out/$opt"
    mkdir -p "$root"/{fixtures,reader}
    flags=("-$opt")
    if [[ $opt == O0 ]]; then flags+=(-fomit-interface-pragmas -fno-write-if-simplified-core); fi
    (
        cd "$root/fixtures"
        cp "$src"/OpenArrowKind{,Matchable,Unmatchable,Client}.hs .
        sed -e 's/OpenArrowKindMatchable/OtherMatchable/g' -e 's/Type %M -> Type/Bool %M -> Bool/' \
            OpenArrowKindMatchable.hs > OtherMatchable.hs
        cat > Generic.hs <<'HS'
{-# LANGUAGE Modifiers, TypeApplications, TypeFamilies #-}
module Generic () where
import Data.Kind (Type)
import OpenArrowKind (OpenKind)
type instance forall m. OpenKind @m = Type %m -> Type
HS
        cat > BareKind.hs <<'HS'
{-# LANGUAGE TypeFamilies #-}
module BareKind (OpenKind) where
import Data.Kind (Type)
type family OpenKind :: Type
HS
        for selector in Matchable Unmatchable; do
            sed -e "s/OpenArrowKind$selector/Bare$selector/g" -e 's/import OpenArrowKind/import BareKind/' \
                -e 's/OpenKind @[MU] =/OpenKind =/' "OpenArrowKind$selector.hs" > "Bare$selector.hs"
        done
        sed -e 's/module OpenArrowKind/module TrailingKind/' -e 's/type family OpenKind @m/type family OpenKind/' \
            OpenArrowKind.hs > TrailingKind.hs
        sed -e 's/OpenArrowKindMatchable/TrailingWriter/g' -e 's/import OpenArrowKind/import TrailingKind/' \
            OpenArrowKindMatchable.hs > TrailingWriter.hs
    )
    compile() {
        local mod=$1 owner=${2:-}
        mkdir -p "$root/$mod"
        (
            cd "$root/$mod"
            cp "$root/fixtures/$mod.hs" .
            if [[ -n $owner ]]; then cp "$root/$owner/$owner.hi" .; fi
            "${ghc[@]}" "${flags[@]}" -c "$mod.hs"
            cp "$mod.hi" ../reader/
        )
    }
    compile OpenArrowKind
    for mod in OpenArrowKindMatchable OpenArrowKindUnmatchable OtherMatchable Generic; do
        compile "$mod" OpenArrowKind
    done
    compile BareKind
    compile BareMatchable BareKind
    compile BareUnmatchable BareKind
    compile TrailingKind
    (
        cd "$root/reader"
        reject() {
            local mod=$1 code=$2
            if "${ghc[@]}" "${flags[@]}" -c "$mod.hs" >reject.log 2>&1 || ! grep -Fq "$code" reject.log; then
                cat reject.log >&2
                exit 1
            fi
        }
        join_writers() {
            local first=$1 second=$2
            cat > Join.hs <<HS
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Join where
import $first ()
import $second ()
HS
        }
        cp "$root/fixtures/"{OpenArrowKindClient,TrailingWriter}.hs .
        "${ghc[@]}" "${flags[@]}" -main-is OpenArrowKindClient -c OpenArrowKindClient.hs
        "${ghc[@]}" "-$opt" OpenArrowKindClient.o \
            ../OpenArrowKind/OpenArrowKind.o ../OpenArrowKindMatchable/OpenArrowKindMatchable.o \
            ../OpenArrowKindUnmatchable/OpenArrowKindUnmatchable.o -o run
        test "$(./run)" = '(Just True,True)'

        join_writers Generic OpenArrowKindMatchable
        "${ghc[@]}" "${flags[@]}" -c Join.hs
        join_writers Generic OpenArrowKindUnmatchable
        reject Join '[GHC-34447]'
        join_writers OtherMatchable OpenArrowKindMatchable
        reject Join '[GHC-34447]'
        join_writers BareMatchable BareUnmatchable
        reject Join '[GHC-34447]'
        reject TrailingWriter '[GHC-45474]'

        cat > WrongKind.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications, TypeFamilies #-}
module WrongKind where
import Generic ()
import GHC.Matchability (U)
import OpenArrowKind (OpenKind)
type family Interpret (b :: Bool) :: OpenKind @U
type instance Interpret 'True = Maybe
HS
        cat > Inverse.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, GADTs, NoExperimentalDependentTypes #-}
{-# LANGUAGE PolyKinds, TypeApplications, TypeFamilies, TypeOperators #-}
module Inverse where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)
import OpenArrowKind (OpenKind)
import OpenArrowKindUnmatchable ()
bad :: forall (f :: OpenKind @U) a b. f a ~ f b => a :~: b
bad = Refl
HS
        reject WrongKind '[GHC-83865]'
        reject Inverse '[GHC-25897]'
        grep -Fq 'In the expression: Refl' reject.log
    )
    printf '%s: open selector sources and overlap checks passed\n' "$opt"
done
