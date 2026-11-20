#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
# Apply these after -O so the retained modules use the stripped interface.
interface_flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
shift 2
ghc+=("$@")
ways=()
if [[ " $* " != *" -dynamic "* ]] && "${ghc[@]}" --info | grep -F '"Support dynamic-too","YES"' >/dev/null; then
    ways=(-dynamic-too)
fi
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
cat >Value.hs <<'HS'
module Value where
import Numeric.Natural (Natural)
{-# NOINLINE score #-}
score :: [Natural] -> Natural
score [] = 7
score (n : _) = n
HS
cat >Rule.hs <<'HS'
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Rule () where
import Value
{-# RULES "score/empty" score [] = 29 #-}
HS
cat >Producer.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Producer where
import Data.Proxy
import Numeric.Natural
import Rule ()
import Value
keep :: foreach (b :: Bool) -> Proxy b
keep _ = Proxy
pair :: Natural -> (Natural, Char)
pair n = (n + score [], error "unused right field")
HS
cat >Main.hs <<'HS'
module Main where
import Data.Proxy
import Producer
import Rule ()
import Value
import Numeric.Natural
observe :: Bool -> (Natural, Natural)
observe b = case keep b of
    Proxy -> (score [], fst (pair 7))
main :: IO ()
main = print (observe (error "unused index"))
HS
"${ghc[@]}" -O1 "${interface_flags[@]}" "${ways[@]}" \
    -ddump-rule-firings -ddump-to-file -fno-dump-with-ways --make Main.hs -o run
grep -Fq 'Rule fired: score/empty (Rule)' Producer.dump-rule-firings
./run

# Compile the proof using the same optimized objects that produced the value.
mkdir client
cp ./*.hi ./*.o "$area/NativeClosedFamilyProjection.hs" client/
if [[ -f Producer.dyn_o ]]; then cp ./*.dyn_o ./*.dyn_hi client/; fi
cd client
"${ghc[@]}" -O0 "${interface_flags[@]}" "${ways[@]}" -c NativeClosedFamilyProjection.hs
cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Proof where
import Data.Type.Equality
import qualified NativeClosedFamilyProjection as F
import qualified Producer as P
proof :: F.pick (P.pair 7) :~: 36
proof = Refl
HS
"${ghc[@]}" -O0 "${interface_flags[@]}" -c Proof.hs
sed 's/:~: 36$/:~: 14/' Proof.hs >Stale.hs
if "${ghc[@]}" -O0 "${interface_flags[@]}" -fforce-recomp -c Stale.hs >stale.log 2>&1; then exit 1; fi
grep -Fq GHC-83865 stale.log || { cat stale.log; exit 1; }
