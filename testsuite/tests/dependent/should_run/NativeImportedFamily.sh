#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -i -i. -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{producer,client}
cd "$out/producer"
cp "$src"/{ImportedFamilyProvider,ImportedFamily}.hs .
"${ghc[@]}" -c ImportedFamilyProvider.hs ImportedFamily.hs

# The consumer must typecheck before it has either of the producer objects.
cp ./*.hi ../client/
cd ../client
cat >Client.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import ImportedFamily (importedFamilyResults)
main :: IO ()
main = print (importedFamilyResults :: (Bool, Ordering, Char))
HS
"${ghc[@]}" -c Client.hs
"${ghc[@]}" Client.o ../producer/{ImportedFamily,ImportedFamilyProvider}.o -o client
./client

expect_cycle() {
    local mod=$1 site
    site=$(awk -v call="$mod.keep" '/^type instance/ { print NR ":" index($0, call); exit }' "$mod.hs")
    if "${ghc[@]}" -c "$mod.hs" >cycle.log 2>&1 ||
       ! grep -Eq "^$mod\.hs:$site(-[0-9]+)?: error: \[GHC-70566\]" cycle.log ||
       ! grep -Fq 'This type instance requires the unfinished local function keep' cycle.log; then
        cat cycle.log >&2
        exit 1
    fi
}

cat >BlindCycle.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies #-}
module BlindCycle where
import Data.Proxy (Proxy(..))
import ImportedFamilyProvider (Payload, hidden)
type instance Payload Int = BlindCycle.keep Bool
keep :: forall a. a -> a
keep x = const x (hidden Proxy)
HS
expect_cycle BlindCycle

sed -e 's/BlindCycle/ErasedCycle/g' \
    -e '/^import Data.Proxy/d' -e 's/(Payload, hidden)/(Payload)/' \
    -e 's/(hidden Proxy)/(True :: Payload Int)/' BlindCycle.hs >ErasedCycle.hs
expect_cycle ErasedCycle

# A hidden dictionary is a genuine dependency even when observation ignores it.
cat >Dictionary.hs <<'HS'
{-# LANGUAGE CPP, TypeFamilies #-}
module Main where
import Data.Proxy (Proxy(..))
import ImportedFamilyProvider (Payload, hidden)
#ifdef WITH_EQUATION
type instance Payload Int = Bool
#endif
keep :: a -> a
keep x = const x (hidden Proxy)
choose :: a -> a -> a
choose x y = if hidden Proxy then x else y
main :: IO ()
main = print (keep False, choose True False)
HS
"${ghc[@]}" -c Dictionary.hs
"${ghc[@]}" Dictionary.o ../producer/ImportedFamilyProvider.o -o without
"${ghc[@]}" -DWITH_EQUATION -fforce-recomp -c Dictionary.hs
"${ghc[@]}" Dictionary.o ../producer/ImportedFamilyProvider.o -o with
without=$(./without)
with=$(./with)
if [[ $without != '(False,False)' || $with != '(False,True)' ]]; then
    printf 'Dictionary control produced %s without the equation and %s with it\n' "$without" "$with" >&2
    exit 1
fi
