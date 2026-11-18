#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -i -fomit-interface-pragmas
     -fno-write-if-simplified-core -fdiagnostics-color=never -dcore-lint -dstg-lint)
run=$(mktemp -d)
trap 'rm -rf -- "$run"' EXIT
cd "$run"
mkdir producer exporter interfaces client
cp "$src/CharacterCase.hs" producer/
cp "$src/CharacterExport.hs" exporter/
cp "$src/NativeOpenCharFamily.hs" client/
(cd producer && "${ghc[@]}" -c CharacterCase.hs)
cp producer/CharacterCase.hi interfaces/
(cd exporter && "${ghc[@]}" -i../interfaces -c CharacterExport.hs)
cp exporter/CharacterExport.hi interfaces/
cd client
"${ghc[@]}" -i../interfaces -c NativeOpenCharFamily.hs
link_and_run() {
    "${ghc[@]}" NativeOpenCharFamily.o ../exporter/CharacterExport.o ../producer/CharacterCase.o -o client
    ./client
}
[[ $(link_and_run) == '[88,201,33]' ]]
"${ghc[@]}" -i../interfaces -c NativeOpenCharFamily.hs -v1 >reuse.log 2>&1
grep -Fxq 'compilation IS NOT required' reuse.log

cat > Invalid.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeApplications #-}
module Invalid where
import CharacterExport (p3)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownChar)
wrong :: Char
wrong = p3 @'x' @'Y' Proxy
rigid :: forall c. KnownChar c => Proxy c -> Char
rigid = p3 @c @c
HS
if "${ghc[@]}" -i../interfaces -c Invalid.hs >invalid.log 2>&1; then
    echo 'accepted an invalid character equality' >&2
    exit 1
fi
grep -Eq '^Invalid.hs:7:.*error:' invalid.log
grep -Eq '^Invalid.hs:9:.*error:' invalid.log
grep -Fq "Couldn't match" invalid.log

# Recompile the same three modules after a body-only producer edit.
cd ../producer
sed "s/upper 'x' = 'X'/upper 'x' = 'Y'/" CharacterCase.hs >changed
mv changed CharacterCase.hs
"${ghc[@]}" -c CharacterCase.hs
cp CharacterCase.hi ../interfaces/
(cd ../exporter && "${ghc[@]}" -i../interfaces -c CharacterExport.hs)
cp ../exporter/CharacterExport.hi ../interfaces/
cd ../client
"${ghc[@]}" -i../interfaces -c NativeOpenCharFamily.hs
[[ $(link_and_run) == '[89,201,33]' ]]

# A forwarding binding has its own name. Its equation still depends on the
# worker. A normal --make build must recheck the unchanged client.
cd "$run"
mkdir forwarding
cd forwarding
cp "$src/CharacterCase.hs" "$src/NativeOpenCharFamily.hs" .
cat > Forward.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Forward (convert) where
import qualified CharacterCase
convert :: Char -> Char
convert = CharacterCase.upper
HS
sed -e 's/import qualified CharacterCase/import qualified Forward/' \
    -e 's/CharacterCase.upper/Forward.convert/' "$src/CharacterExport.hs" >CharacterExport.hs
"${ghc[@]}" -i. --make NativeOpenCharFamily.hs -o run
[[ $(./run) == '[88,201,33]' ]]
sed "s/upper 'x' = 'X'/upper 'x' = 'Y'/" "$src/CharacterCase.hs" >CharacterCase.hs
"${ghc[@]}" -i. --make NativeOpenCharFamily.hs -o run
[[ $(./run) == '[89,201,33]' ]]

# Confirm a conflicting rule fires on the term before checking its type use.
cd "$run"
mkdir rules
cd rules
cp "$src"/Character{Case,Export}.hs .
printf '\n{-# NOINLINE upper #-}\n' >> CharacterCase.hs
cat > Orphan.hs <<'HS'
{-# OPTIONS_GHC -Wno-orphans #-}
module Orphan () where
import CharacterCase (upper)
{-# RULES "change upper" forall c. upper c = 'Z' #-}
HS
cat > Runtime.hs <<'HS'
module Main where
import CharacterCase (upper)
import Orphan ()
main :: IO ()
main = print (upper 'x')
HS
sed '/^import CharacterExport/a import Orphan ()' "$src/NativeOpenCharFamily.hs" >Client.hs
rules=("${ghc[@]}" -i. -O1 -fno-omit-interface-pragmas -fno-ignore-interface-pragmas)
"${rules[@]}" --make Runtime.hs -o runtime
[[ $(./runtime) == "'Z'" ]]
if "${rules[@]}" --make Client.hs -o stale >invalid.log 2>&1; then
    echo 'accepted character evidence after rewriting its producer' >&2
    exit 1
fi
grep -Fq '[GHC-83605]' invalid.log
grep -Fq 'CharacterCase.upper' invalid.log
