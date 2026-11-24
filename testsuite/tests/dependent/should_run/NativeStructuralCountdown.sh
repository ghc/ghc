#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

cat >"$work/Countdown.hs" <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Countdown (Step(..), countdown, empty) where

import Data.Type.Equality ((:~:)(Refl))

data Step = Zero | Next Step

countdown :: Step -> [Step]
countdown Zero = []
countdown (Next rest) = rest : countdown rest

empty :: countdown 'Zero :~: ('[] :: [Step])
empty = Refl
HS
cat >"$work/Main.hs" <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Main where

import Countdown
import Data.Type.Equality ((:~:)(Refl))

next :: foreach (n :: Step) -> countdown ('Next n) :~: (n ': countdown n)
next _ = Refl

#ifdef WRONG
wrong :: foreach (n :: Step) -> countdown ('Next n) :~: ('Next n ': countdown n)
wrong _ = Refl
#endif

main :: IO ()
main = case (empty, next Zero, countdown (Next (error "predecessor"))) of
    (Refl, Refl, _ : _) -> putStrLn "Refl"
    _ -> fail "expected a cons"
HS

for opt in -O0 -O2; do
    mkdir -p "$work/$opt"/{provider,reader}
    cd "$work/$opt/provider"
    cp "$work/Countdown.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c Countdown.hs
    cp Countdown.{hi,o} ../reader/
    cd ../reader
    cp "$work/Main.hs" .
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o Countdown.o -o check
    ./check >actual
    grep -Fxq Refl actual
    if "${ghc[@]}" "$opt" -DWRONG -fforce-recomp -c Main.hs >wrong.log 2>&1 ||
       ! grep -Fq 'error: [GHC-83865]' wrong.log; then
        cat wrong.log >&2
        exit 1
    fi
done
