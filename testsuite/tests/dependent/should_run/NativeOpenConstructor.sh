#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -dcore-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{worker,exporter,client}
cd "$out/worker"
cp "$src/OpenConstructorWorker.hs" .
"${ghc[@]}" -c OpenConstructorWorker.hs
"${ghc[@]}" --show-iface OpenConstructorWorker.hi > worker.iface
awk '/^  routeCached ::/{body=1;next} body && /^[0-9a-f]+$/{exit} body{print}' \
    worker.iface > route-cached.iface
if [[ $(grep -c 'cachedPick' route-cached.iface) -lt 2 ]]; then
    cat route-cached.iface >&2
    printf '%s\n' 'The cache control needs two calls to cachedPick in its unfolding.' >&2
    exit 1
fi
cp OpenConstructorWorker.hi ../exporter/
cd ../exporter
cp "$src/OpenConstructorSource.hs" .
"${ghc[@]}" -c OpenConstructorSource.hs
cp OpenConstructorSource.hi OpenConstructorWorker.hi ../client/

expect_rejection() {
    local mod=$1 line=$2 code=$3 message=$4
    if "${ghc[@]}" -c "$mod.hs" > reject.log 2>&1 ||
       ! grep -Eq "^$mod\\.hs:$line:[0-9]+(-[0-9]+)?: error:" reject.log ||
       ! grep -Eq "$code" reject.log ||
       ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

cat > Blocked.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module Blocked where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified OpenConstructorWorker as W
blocked :: forall (p :: W.Choice Type Type). Proxy p -> W.pick p
blocked _ = undefined
SOURCE
expect_rejection Blocked 6 '\[GHC-70566\]' 'rigid argument p'

cat > Strict.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module Strict where
import qualified OpenConstructorWorker as W
strict :: forall a b. a -> b -> W.demand ('W.LeftFields a b)
strict _ y = y
SOURCE
expect_rejection Strict 4 '\[GHC-70566\]' 'rigid argument a'

cat > WrongField.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module WrongField where
import qualified OpenConstructorWorker as W
wrong :: forall a b. a -> b -> W.pick ('W.RightFields b a)
wrong x _ = x
SOURCE
expect_rejection WrongField 5 '\[GHC-(25897|83865)\]' 'x'

cat > DisjointRule.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module DisjointRule where
import qualified OpenConstructorWorker as W
direct :: forall a b. a -> b -> W.pick ('W.LeftFields a b)
direct _ y = y
throughHelper :: forall a b. a -> b -> W.route ('W.RightFields b a)
throughHelper _ y = y
{-# RULES "constructor/disjoint" forall (shell :: W.Choice Bool Char).
    W.pick shell = 'z' #-}
SOURCE
"${ghc[@]}" -c DisjointRule.hs

cat > OpenRule.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module OpenRule where
import Data.Kind (Type)
import qualified OpenConstructorWorker as W
pick :: forall a b. a -> b -> W.routeCached ('W.LeftFields a b)
pick _ y = y
{-# RULES "constructor/matching" forall (shell :: W.Choice Type Type).
    W.cachedPick shell = undefined #-}
SOURCE
expect_rejection OpenRule 8 '\[GHC-83605\]' 'rewrite rule can change the result'

cd ../client
cp "$src/NativeOpenConstructor.hs" .
"${ghc[@]}" -c NativeOpenConstructor.hs
"${ghc[@]}" NativeOpenConstructor.o ../exporter/OpenConstructorSource.o ../worker/OpenConstructorWorker.o -o client
./client


# Change only field strictness. The unchanged exporter must recheck even
# though the worker's source body and its ordinary type stay the same.
cd ../worker
"${ghc[@]}" --show-iface OpenConstructorWorker.hi > original.iface
sed 's/LeftFields a b/LeftFields !a b/' "$src/OpenConstructorWorker.hs" > OpenConstructorWorker.hs
"${ghc[@]}" -c OpenConstructorWorker.hs
"${ghc[@]}" --show-iface OpenConstructorWorker.hi > changed.iface
grep -F 'ABI hash:' original.iface > original.hash
grep -F 'ABI hash:' changed.iface > changed.hash
if cmp -s original.hash changed.hash; then
    printf '%s\n' 'Changing field strictness did not change the worker interface ABI hash.' >&2
    exit 1
fi
cp OpenConstructorWorker.hi ../exporter/
cd ../exporter
expect_rejection OpenConstructorSource 9 '\[GHC-70566\]' 'ordinary boxed constructors with lazy fields'
