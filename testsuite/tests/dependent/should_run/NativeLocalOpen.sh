#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -dcore-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{producer,client}
cd "$out/producer"
cp "$src/NativeLocalOpenProducer.hs" .
"${ghc[@]}" -c NativeLocalOpenProducer.hs
cp NativeLocalOpenProducer.hi ../client/
cd ../client
cp "$src/NativeLocalOpen.hs" .
"${ghc[@]}" -c NativeLocalOpen.hs
"${ghc[@]}" NativeLocalOpen.o ../producer/NativeLocalOpenProducer.o -o client
./client

cd ../producer
sed 's/^backward x _ = x$/backward _ y = y/' NativeLocalOpenProducer.hs >changed
mv changed NativeLocalOpenProducer.hs
"${ghc[@]}" -c NativeLocalOpenProducer.hs
cp NativeLocalOpenProducer.hi ../client/
cd ../client
if "${ghc[@]}" -c NativeLocalOpen.hs >changed.log 2>&1 ||
   ! grep -Fq 'GHC-83865' changed.log || ! grep -Fq 'backward = True' changed.log; then
    cat changed.log >&2
    exit 1
fi

cd ../producer
expect_rejection() {
    local mod=$1 line=$2 code=$3 message=$4
    if "${ghc[@]}" -fforce-recomp -c "$mod.hs" > reject.log 2>&1 ||
       ! grep -Eq "^$mod\\.hs:$line:[0-9]+(-[0-9]+)?: error:" reject.log ||
       ! grep -Eq "$code" reject.log ||
       ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

cat > NativeLocalOpenCycle.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
module NativeLocalOpenCycle where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenCycle.keep a
keep :: a -> Result a
keep x = x
SOURCE
expect_rejection NativeLocalOpenCycle 5 '\[GHC-70566\]' 'dependency cycle'

cat > NativeLocalOpenStrict.hs <<'SOURCE'
{-# LANGUAGE BangPatterns, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module NativeLocalOpenStrict where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenStrict.keep a
keep :: a -> a
keep !x = x
SOURCE
expect_rejection NativeLocalOpenStrict 5 '\[GHC-70566\]' 'rigid argument a'

cat > NativeLocalOpenSplice.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures, TemplateHaskell #-}
module NativeLocalOpenSplice where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenSplice.keep a
$(pure [])
keep :: a -> a
keep x = x
SOURCE
expect_rejection NativeLocalOpenSplice 5 '\[GHC-76037\]' 'NativeLocalOpenSplice.keep'

cat > NativeLocalOpenStale.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeLocalOpenStale where
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
size :: [Natural] -> Natural
size xs = fromIntegral (length xs)
SOURCE
"${ghc[@]}" -c NativeLocalOpenStale.hs
test -s NativeLocalOpenStale.o
cat >> NativeLocalOpenStale.hs <<'SOURCE'
proof :: NativeLocalOpenStale.size '[1,2] :~: 2
proof = Refl
SOURCE
expect_rejection NativeLocalOpenStale 7 '\[GHC-70565\]' \
    'Closed type evaluation requires an imported definition.'
