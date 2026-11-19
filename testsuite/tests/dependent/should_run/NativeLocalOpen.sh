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
sed 's/^select x _ = x$/select _ y = y/' NativeLocalOpenProducer.hs >changed
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

cat > NativeLocalOpenNames.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, ExplicitForAll, TypeApplications #-}
module NativeLocalOpenNames where
pick x = x
explicit :: forall a. a -> pick a
explicit x = x
implicit :: pick a -> pick a
implicit x = x
twoBinders :: Maybe Bool
twoBinders = implicit @Maybe @Bool (Just True)
SOURCE
"${ghc[@]}" -c NativeLocalOpenNames.hs

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

cat > NativeLocalOpenTermCycle.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
module NativeLocalOpenTermCycle where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenTermCycle.choose a
choose x = loop x
loop x = loop x
SOURCE
expect_rejection NativeLocalOpenTermCycle 5 '\[GHC-70566\]' 'Reduction exceeded the open evaluation step limit'

cat > NativeLocalOpenHelperType.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
module NativeLocalOpenHelperType where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenHelperType.choose a
type Payload a = a
choose x = helper x
helper :: a -> Result a
helper x = x
SOURCE
expect_rejection NativeLocalOpenHelperType 5 '\[GHC-70566\]' 'dependency cycle through helper and Result'
sed 's/^helper :: a -> Result a$/helper :: a -> Payload a/' NativeLocalOpenHelperType.hs >changed
mv changed NativeLocalOpenHelperType.hs
expect_rejection NativeLocalOpenHelperType 5 '\[GHC-70566\]' 'same-group declaration Payload'

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

cat > NativeLocalOpenRules.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module NativeLocalOpenRules where
import Data.Kind (Type)
type Selected :: Type -> Type -> Type
type Selected a b = NativeLocalOpenRules.choose a b
choose :: a -> a -> a
choose x y = select x (unused y)
select :: a -> a -> a
select x _ = x
unused :: a -> a
unused x = unused x
{-# RULES "unused/strict" forall x. unused x = x `seq` x #-}
SOURCE
"${ghc[@]}" -c NativeLocalOpenRules.hs
cat >>NativeLocalOpenRules.hs <<'SOURCE'
{-# RULES "select/second" forall x y. select x y = y #-}
SOURCE
expect_rejection NativeLocalOpenRules 14 '\[GHC-83605\]' 'select'

cp "$src/NativeLocalOpenSibling.hs" .
"${ghc[@]}" -c NativeLocalOpenSibling.hs
sed -e 's/^left x = keep x (right x)$/left x = right x/' \
    -e 's/^right x = left x$/right x = keep x (left x)/' NativeLocalOpenSibling.hs >changed
mv changed NativeLocalOpenSibling.hs
expect_rejection NativeLocalOpenSibling 10 '\[GHC-83605\]' 'right'

cat > NativeLocalOpenPriorHelper.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures, TemplateHaskell #-}
module NativeLocalOpenPriorHelper where
import Data.Kind (Type)
choose :: a -> a
choose x = helper x
helper :: a -> a
helper = $$([|| \x -> x ||])
$(pure [])
type Selected :: Type -> Type
type Selected a = NativeLocalOpenPriorHelper.choose a
SOURCE
expect_rejection NativeLocalOpenPriorHelper 10 '\[GHC-70566\]' 'pending typed splice'

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
