#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

reject() {
    local code=$1
    shift
    local status=0
    "${ghc[@]}" "$@" >negative.log 2>&1 || status=$?
    if [[ "$status" != 1 ]] || ! grep -Fq "[GHC-$code]" negative.log; then
        cat negative.log >&2
        return 1
    fi
}

for opt in -O0 -O2; do
    mkdir -p "$work/$opt"/{provider,reader}
    cd "$work/$opt/provider"
    cp "$src/NativeLocalBooleanResult.hs" .
    provider=("$opt" -fomit-interface-pragmas -fno-write-if-simplified-core
              -fforce-recomp -c NativeLocalBooleanResult.hs)
    "${ghc[@]}" "${provider[@]}"
    "${ghc[@]}" -fprint-explicit-foralls --show-iface NativeLocalBooleanResult.hi >provider.iface
    grep -Fq 'SourceTable T:substitute' provider.iface
    grep -Fq 'SourceTable T:same' provider.iface
    cp NativeLocalBooleanResult.{hi,o} ../reader/
    reject 68947 "${provider[@]}" -DPARTIAL
    reject 68947 "${provider[@]}" -DNONDECREASING
    reject 70566 "${provider[@]}" -DABSTRACT
    if [[ "$opt" == -O2 ]]; then reject 83605 "${provider[@]}" -DRULE; fi
    cd ../reader
    test ! -e NativeLocalBooleanResult.hs
    cat >Main.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where
import NativeLocalBooleanResult

#ifdef FALSE_TARGET
step :: Step 'Z ('Nest ('Var 'Z)) 'Done ('Nest ('Var 'Z))
#else
step :: Step 'Z ('Nest ('Var 'Z)) 'Done ('Nest 'Done)
#endif
step = Reduce

other :: Step ('S 'Z) ('Var 'Z) 'Done ('Var 'Z)
other = Reduce

main :: IO ()
main = case (step, other) of (Reduce, Reduce) -> putStrLn "checked substitution"
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o NativeLocalBooleanResult.o -o check
    ./check >actual
    printf 'checked substitution\n' | diff -u - actual
    reject 83865 "$opt" -DFALSE_TARGET -fforce-recomp -c Main.hs
done
