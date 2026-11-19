#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"
g=("$1" "-B$2" -v0 -package-env - -fdiagnostics-color=never)

reject() {
    local status=0
    "${g[@]}" "$@" >err.log 2>&1 || status=$?
    if [[ $status != 1 ]]; then
        cat err.log >&2
        exit 1
    fi
}

cat >Mismatch.hs <<'HS'
{-# LANGUAGE DataKinds, TypeApplications, TypeOperators #-}
module Mismatch where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, M, U, Multiplicity(One))
bad :: FUN @M 'One Int Bool :~: FUN @U 'One Int Bool
bad = Refl
HS
reject -XLinearTypes -XModifiers -c Mismatch.hs
grep -Fq 'error: [GHC-83865]' err.log
grep -Fq 'Int %M %1 -> Bool' err.log
reject -XLinearTypes -XNoModifiers -c Mismatch.hs
grep -Fq 'Int %1 -> @(M) Bool' err.log

cat >MapKind.hs <<'HS'
{-# LANGUAGE DataKinds, Modifiers, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module MapKind (Map) where
import GHC.Exts (Matchability(Unmatchable))
type Map :: (a %Unmatchable -> b) -> [a] -> [b]
type family Map f xs where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs
HS
"${g[@]}" -c MapKind.hs
"${g[@]}" --show-iface MapKind.hi >old.iface
"${g[@]}" -XModifiers -fprint-explicit-matchabilities --show-iface MapKind.hi >new.iface
cmp old.iface new.iface

cat >kind.ghci <<'GHCI'
:set -XModifiers -dppr-cols=240
:load MapKind.hs
import GHC.Exts (Matchability(Unmatchable))
:kind Map
:set -fprint-explicit-matchabilities
:kind Map
:quit
GHCI
if ! "${g[@]}" --interactive -ignore-dot-ghci -ghci-script kind.ghci >kind.log 2>&1; then
    cat kind.log >&2
    exit 1
fi
implicit=$(sed -n 's/^Map :: //p' kind.log | head -1)
explicit=$(sed -n 's/^Map :: //p' kind.log | tail -1)
if [[ "$implicit" != '(a -> b) -> [a] -> [b]' || "$explicit" != *'%Unmatchable'* ]]; then
    cat kind.log >&2
    exit 1
fi

cat >Readback.hs <<HS
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll, Modifiers #-}
{-# LANGUAGE PolyKinds, StandaloneKindSignatures, TypeFamilies, TypeOperators #-}
module Readback where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (Matchability(Unmatchable), U)

type Recovered :: $explicit
type family Recovered f xs where
    Recovered f '[] = '[]
    Recovered f (x ': xs) = f x ': Recovered f xs

type Callback :: Type %U -> Type
type family Callback a where
    Callback Int = Char

checked :: Recovered Callback '[Int] :~: '[Char]
checked = Refl
HS
"${g[@]}" -dcore-lint -dstg-lint -c Readback.hs
sed -e 's/^module Readback where/module Lost where/' \
    -e "s/^type Recovered :: .*/type Recovered :: $implicit/" Readback.hs >Lost.hs
reject -c Lost.hs
grep -Fq 'Lost.hs:17:22: error: [GHC-83865]' err.log
grep -Fq Unmatchable err.log
