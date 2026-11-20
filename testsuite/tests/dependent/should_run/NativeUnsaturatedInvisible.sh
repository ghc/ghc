#!/usr/bin/env bash
set -euo pipefail
dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider client
cp "$dir/InvisibleFamily.hs" provider/
cp "$dir"/NativeUnsaturatedInvisible{Consumer,Inverse,Matchable}.hs client/
ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never)
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
   [[ $(grep -Fc '[GHC-25897]' inverse.log) -ne 2 ]]; then
    cat inverse.log >&2
    exit 1
fi
if "${ghc[@]}" -c NativeUnsaturatedInvisibleMatchable.hs > matchable.log 2>&1 ||
   [[ $(grep -Fc '[GHC-83865]' matchable.log) -ne 3 ]] ||
   ! grep -Fq 'type Bad :: forall k. @M k' matchable.log ||
   ! grep -Fq "type BadVisible = ExpectsM (PickVisible 'True)" matchable.log ||
   ! grep -Fq "type BadSuffix = ExpectsM (ReadAt 'True)" matchable.log ||
   ! grep -Fq 'Expected kind ‘forall k. @M' matchable.log ||
   ! grep -Eq 'has kind ‘forall k\. @([^[:space:]]*\.)?U([[:space:]]|$)' matchable.log; then
    cat matchable.log >&2
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
