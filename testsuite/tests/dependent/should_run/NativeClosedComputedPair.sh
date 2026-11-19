#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -package-env - -fomit-interface-pragmas
     -fno-write-if-simplified-core -fdiagnostics-color=never -dcore-lint -dstg-lint)
shift 2
ghc+=("$@")
ways=()
if [[ " $* " != *" -dynamic "* ]] && "${ghc[@]}" --info | grep -F '"Support dynamic-too","YES"' >/dev/null; then
    ways=(-dynamic-too)
fi
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
mkdir "$work"/{main,interfaces,legacy,replacement}
cd "$work/main"
cp "$src"/{ComputedPair,PairProjection,ComputedPairProof}.hs .

compile() {
    "${ghc[@]}" -i -i. "$@"
}

producer() {
    compile "${ways[@]}" -c "$@"
}

reject() {
    local message=$1
    shift
    if compile "$@" >error.log 2>&1 || ! grep -Fq "$message" error.log; then
        cat error.log >&2
        exit 1
    fi
}

# First check against the configured compiler's freshly built library.
producer ComputedPair.hs PairProjection.hs
mv PairProjection.o ../
if [[ -f PairProjection.dyn_o ]]; then mv PairProjection.dyn_o ../; fi
compile -c ComputedPairProof.hs
cp ./*.hi ../interfaces/
if compgen -G '*.dyn_hi' >/dev/null; then cp ./*.dyn_hi ../interfaces/; fi
cat >../interfaces/UseAlias.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module UseAlias where
import ComputedPairProof (Selected, SelectedRight)
import Data.Type.Equality ((:~:)(Refl))
proof :: Selected :~: 8
proof = Refl
rightProof :: SelectedRight :~: 8
rightProof = Refl
HS
(cd ../interfaces && compile -c UseAlias.hs)
sed -e 's/module ComputedPairProof .* where/module ObserveRight where/' \
    -e 's/F.pick (W.bump 7) :~: 8/F.pickRight (W.bump 7) :~: '\''x'\''/' \
    "$src/ComputedPairProof.hs" >ObserveRight.hs
reject 'unread' -c ObserveRight.hs

cat >ObserveLeft.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module ObserveLeft where
import qualified ComputedPair as W
import Data.Type.Equality ((:~:)(Refl))
import qualified PairProjection as F
proof :: F.pick (W.bumpRight 7) :~: 'x'
proof = Refl
HS
reject 'unread left field' -c ObserveLeft.hs

cat >RepeatPartial.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RoleAnnotations, ScopedTypeVariables #-}
module RepeatPartial where
import qualified ComputedPair as W
import Numeric.Natural (Natural)
import qualified PairProjection as F
data N (n :: Natural) = N
type role N nominal
{-# RULES "pick/result" forall (p :: (Natural, Char)). F.pick p = 17 #-}
same :: N (F.pick (W.bump 7)) -> N (F.pick (W.bump 7))
same x = x
HS
rule=(-fenable-rewrite-rules -Wno-inline-rule-shadowing)
reject '[GHC-83605]' "${rule[@]}" -c RepeatPartial.hs

cat >../interfaces/AliasValues.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, RoleAnnotations #-}
module AliasValues (value, consume) where
import ComputedPairProof (Selected)
import Numeric.Natural (Natural)
data N (n :: Natural) = N
type role N nominal
value :: N Selected
value = N
consume :: N Selected -> Bool
consume _ = True
HS
cat >../interfaces/UseAliasValues.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, ScopedTypeVariables #-}
module UseAliasValues where
import qualified AliasValues as A
import Numeric.Natural (Natural)
import qualified PairProjection as F
{-# RULES "pick/result" forall (p :: (Natural, Char)). F.pick p = 17 #-}
result :: Bool
result = A.consume A.value
HS
(
    cd ../interfaces
    compile -c AliasValues.hs
    rm AliasValues.o
    compile "${rule[@]}" -c UseAliasValues.hs
)

# Model a legacy object on ELF by localizing only the generated closure.
# Keep the ordinary worker executable. The optional symbol lookup should
# preserve the loaded module and use full readback.
if [[ $(uname -s) != Linux ]] || ! command -v nm >/dev/null || ! command -v objcopy >/dev/null; then
    exit 0
fi
cat >Legacy.hs <<'HS'
module Legacy (bump) where
import Numeric.Natural (Natural)
bump :: Natural -> (Natural, Char)
bump n = (n + 1, 'x')
HS
cp Legacy.hs ../legacy/
(cd ../legacy && producer Legacy.hs)
for obj in ../legacy/Legacy.o ../legacy/Legacy.dyn_o; do
    if [[ ! -f $obj ]]; then continue; fi
    symbol=$(nm -g --defined-only "$obj" | awk '$NF ~ /readback2ZC1ZCbump_closure$/ {print $NF}')
    [[ -n $symbol && $symbol != *$'\n'* ]]
    objcopy --localize-symbol "$symbol" "$obj"
done
producer Legacy.hs
for obj in ../legacy/Legacy.o ../legacy/Legacy.dyn_o; do
    if [[ -f $obj ]]; then cp "$obj" .; fi
done
sed -e 's/module RepeatPartial where/module LegacyClient where/' \
    -e 's/import qualified ComputedPair as W/import qualified Legacy as W/' \
    -e "s/-> N (F.pick (W.bump 7))/-> N (F.pick '(8, 'x'))/" RepeatPartial.hs >LegacyClient.hs
if ! compile "${rule[@]}" -c LegacyClient.hs >legacy.log 2>&1; then
    cat legacy.log >&2
    exit 1
fi
compile --show-iface LegacyClient.hi >legacy.iface
grep -Eq 'addDependentFile ".*Legacy\.(dyn_o|o)"' legacy.iface

# Replace the object alone with a new body and keep its interface fixed.
sed "s/(n + 1, 'x')/(n + 2, 'z')/" Legacy.hs >../replacement/Legacy.hs
(cd ../replacement && producer Legacy.hs)
for obj in ../replacement/Legacy.o ../replacement/Legacy.dyn_o; do
    if [[ -f $obj ]]; then cp "$obj" .; fi
done
reject '[GHC-83605]' "${rule[@]}" -c LegacyClient.hs
sed -i "s/'(8, 'x')/'(9, 'z')/" LegacyClient.hs
compile "${rule[@]}" -c LegacyClient.hs

# An installed native client must not link an optional transitive recipe.
# Work with a fresh new interface and an old object that has its ordinary symbol.
cat >RelayLeaf.hs <<'HS'
module RelayLeaf (field) where
import Numeric.Natural (Natural)
{-# OPAQUE field #-}
field :: Natural -> Char
field _ = error "relay sibling"
HS
cat >RelayWorker.hs <<'HS'
module RelayWorker (bump) where
import Numeric.Natural (Natural)
import qualified RelayLeaf
bump :: Natural -> (Natural, Char)
bump n = (n + 1, RelayLeaf.field n)
HS
producer RelayLeaf.hs RelayWorker.hs
sed "s/error \"relay sibling\"/'x'/" RelayLeaf.hs >../replacement/RelayLeaf.hs
(cd ../replacement && producer RelayLeaf.hs)
for obj in ../replacement/RelayLeaf.o ../replacement/RelayLeaf.dyn_o; do
    if [[ ! -f $obj ]]; then continue; fi
    symbol=$(nm -g --defined-only "$obj" | awk '$NF ~ /readback2.*field_closure$/ {print $NF}')
    [[ -n $symbol && $symbol != *$'\n'* ]]
    if nm -u RelayWorker.o | grep -F "$symbol"; then exit 1; fi
    objcopy --localize-symbol "$symbol" "$obj"
    cp "$obj" .
done
cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import qualified RelayWorker
main :: IO ()
main = print (RelayWorker.bump 7)
HS
compile -c Main.hs
compile Main.o RelayWorker.o RelayLeaf.o -o ordinary
[[ $(./ordinary) == "(8,'x')" ]]
sed -e 's/module LegacyClient where/module RelayClient where/' \
    -e 's/import qualified Legacy as W/import qualified RelayWorker as W/' \
    -e "s/'(9, 'z')/'(8, 'x')/" LegacyClient.hs >RelayClient.hs
if ! compile "${rule[@]}" -c RelayClient.hs >relay.log 2>&1 ||
   [[ -s relay.log ]]; then
    cat relay.log >&2
    exit 1
fi
compile --show-iface RelayClient.hi >relay.iface
grep -Eq 'addDependentFile ".*RelayLeaf\.(dyn_o|o)"' relay.iface

# Optimistic linking still returns a real miss for optional entries.
if ! compile -fexternal-interpreter -opti+RTS -opti--optimistic-linking -opti-RTS \
    "${rule[@]}" -fforce-recomp -c RelayClient.hs >optimistic.log 2>&1 ||
    [[ -s optimistic.log ]]; then
    cat optimistic.log >&2
    exit 1
fi

# A missing required symbol gives a diagnostic at the source call.
for obj in RelayLeaf.o RelayLeaf.dyn_o; do
    if [[ -f $obj ]]; then objcopy --localize-symbol RelayLeaf_field_closure "$obj"; fi
done
reject 'RelayClient.hs:' "${rule[@]}" -c RelayClient.hs
grep -Fq '[GHC-70565]' error.log
grep -Fq 'RelayLeaf_field_closure' error.log
