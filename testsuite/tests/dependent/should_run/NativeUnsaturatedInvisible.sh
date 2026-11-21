#!/usr/bin/env bash
set -euo pipefail
dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider client
cp "$dir/InvisibleFamily.hs" provider/
cp "$dir"/NativeUnsaturatedInvisible{Consumer,Inverse,Matchable}.hs client/
ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never
     -fomit-interface-pragmas -fno-write-if-simplified-core)
(cd provider && "${ghc[@]}" -c InvisibleFamily.hs)
"${ghc[@]}" -dppr-cols=200 -fprint-explicit-kinds --show-iface provider/InvisibleFamily.hi >provider.iface
grep -Fq 'type Inferred :: forall {k}.' provider.iface
grep -Fq 'type Inferred @{k} = Select @k' provider.iface
grep -Fq 'type Test = Carrier Select' provider.iface
grep -Fq "type ObservedVisible = RunVisible @(*) (PickVisible 'GHC.Internal.Types.True)" provider.iface
grep -Fq 'type IndexedVisible flag = RunVisible @(*) (PickVisible flag)' provider.iface
cp provider/InvisibleFamily.hi client/
cd client
"${ghc[@]}" -c NativeUnsaturatedInvisibleConsumer.hs
if "${ghc[@]}" -c NativeUnsaturatedInvisibleInverse.hs > inverse.log 2>&1 ||
   ! grep -Fq "bad = Refl" inverse.log ||
   ! grep -Fq "badVisible = Refl" inverse.log ||
   ! grep -Fq "badRequired = Refl" inverse.log ||
   [[ $(grep -Fc '[GHC-25897]' inverse.log) -ne 2 ]] ||
   [[ $(grep -Fc '[GHC-83865]' inverse.log) -ne 1 ]]; then
    cat inverse.log >&2
    exit 1
fi
if "${ghc[@]}" -c NativeUnsaturatedInvisibleMatchable.hs > matchable.log 2>&1 ||
   [[ $(grep -Fc '[GHC-83865]' matchable.log) -ne 4 ]] ||
   ! grep -Fq 'type BadRequired = ExpectsRequiredM Required' matchable.log ||
   ! grep -Fq 'type Bad :: forall k. @M k' matchable.log ||
   ! grep -Fq "type BadVisible = ExpectsM (PickVisible 'True)" matchable.log ||
   ! grep -Fq "type BadSuffix = ExpectsM (ReadAt 'True)" matchable.log ||
   ! grep -Fq 'Expected kind ‘forall k. @M' matchable.log ||
   ! grep -Eq 'has kind ‘forall k\. @([^[:space:]]*\.)?U([[:space:]]|$)' matchable.log; then
    cat matchable.log >&2
    exit 1
fi
cat >RequiredOff.hs <<'HS'
{-# LANGUAGE Modifiers, NoExperimentalDependentTypes, PolyKinds, RankNTypes, StandaloneKindSignatures #-}
module RequiredOff where
import Data.Kind (Type)
import GHC.Exts (U)
import InvisibleFamily (Required)

type Carry :: (forall k -> @U k %U -> Type) -> Type
data Carry f

bad :: Carry Required
bad = undefined
HS
if "${ghc[@]}" -c RequiredOff.hs >required-off.log 2>&1 ||
   ! grep -Fq '[GHC-27346]' required-off.log ||
   ! grep -Fq 'to write a partial type family application.' required-off.log ||
   ! grep -Fq 'bad :: Carry Required' required-off.log; then
    cat required-off.log >&2
    exit 1
fi
cd ../provider
sed -i "s/Select @Bool = 'True/Select @Bool = 'False/" InvisibleFamily.hs
"${ghc[@]}" -fforce-recomp -c InvisibleFamily.hs
cp InvisibleFamily.hi ../client/
cd ../client
if "${ghc[@]}" -c NativeUnsaturatedInvisibleConsumer.hs >consumer.log 2>&1 ||
   ! grep -Fq "observed = Refl" consumer.log ||
   ! grep -Fq 'False' consumer.log; then
    cat consumer.log >&2
    exit 1
fi

# Restore Select and change only the equation for the visible prefix.
cd ../provider
sed -i -e "s/Select @Bool = 'False/Select @Bool = 'True/" \
       -e "s/PickVisible 'True @Bool = Int/PickVisible 'True @Bool = Char/" InvisibleFamily.hs
"${ghc[@]}" -fforce-recomp -c InvisibleFamily.hs
cp InvisibleFamily.hi ../client/
cd ../client
if "${ghc[@]}" -c NativeUnsaturatedInvisibleConsumer.hs >consumer.log 2>&1 ||
   ! grep -Fq 'observedVisible = Refl' consumer.log ||
   ! grep -Fq 'Char' consumer.log; then
    cat consumer.log >&2
    exit 1
fi
