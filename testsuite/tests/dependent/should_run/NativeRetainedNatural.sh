#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

for opt in O0 O1 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/RetainedNaturals.hs" .
    "${ghc[@]}" "-$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c RetainedNaturals.hs
    cp RetainedNaturals.hi "$src/RetainedNaturalClient.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c RetainedNaturalClient.hs
    cat >Main.hs <<'HS'
module Main where
import RetainedNaturalClient

main :: IO ()
main
  | observeNaturals == (5, 18446744073709551616)
  , observeNamedNatural 18446744073709551617 == 18446744073709551617
  = pure ()
  | otherwise = fail "wrong retained Natural value"
HS
    "${ghc[@]}" "-$opt" -c Main.hs
    "${ghc[@]}" Main.o RetainedNaturalClient.o ../provider/RetainedNaturals.o -o run
    ./run
done
cat >Reject.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, FlexibleContexts, NoExperimentalDependentTypes #-}
#ifdef CUSTOM
{-# LANGUAGE RebindableSyntax #-}
#endif
module Reject where
import Numeric.Natural (Natural)
import RetainedNaturals
#ifdef CUSTOM
import Prelude hiding (fromInteger)
fromInteger :: Integer -> Natural
fromInteger _ = error "custom conversion"
#endif

#if defined(MISMATCH)
wrong :: Reading 6
#elif defined(GIVEN)
wrong :: Num Natural => Reading 5
#else
wrong :: Reading 5
#endif
#if defined(INTEGER)
wrong = rememberNatural (5 :: Integer)
#else
wrong = rememberNatural 5
#endif
HS
for mode in MISMATCH INTEGER CUSTOM GIVEN; do
    if "${ghc[@]}" "-D$mode" -c Reject.hs >"$mode.log" 2>&1; then
        echo "Accepted invalid Natural operand ($mode)"
        exit 1
    fi
    case "$mode" in
        MISMATCH) grep -Fq 'GHC-83865' "$mode.log" ;;
        INTEGER) grep -Fq 'Actual: Integer' "$mode.log" ;;
        CUSTOM) grep -Fq 'has no checked zero and successor equations' "$mode.log" ;;
        GIVEN) grep -Fq 'needs a known' "$mode.log" ;;
    esac
done
