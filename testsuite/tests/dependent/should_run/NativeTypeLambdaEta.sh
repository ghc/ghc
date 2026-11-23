#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

ghc=$1
libdir=$2
mode=${3:-candidate}
run=("$ghc" "-B$libdir" -v0 -package-env - -i -i. -fdiagnostics-color=never
     -dcore-lint -dstg-lint)
root=$PWD
mkdir eta-results
cd eta-results
result=$PWD/results.tsv

for opt in O0 O2; do
    mkdir "$opt"
    (
        cd "$opt"
        mkdir provider named mixed
        cp "$root/NativeTypeLambdaEtaLib.hs" provider/
        (cd provider; "${run[@]}" "-$opt" -fomit-interface-pragmas -c NativeTypeLambdaEtaLib.hs)
        for client in named mixed; do
            cp "$root/NativeTypeLambdaEta.hs" provider/*.hi "$client/"
        done
        (cd named; "${run[@]}" "-$opt" -DNAMED -c NativeTypeLambdaEta.hs)
        printf '%s\tnamed\t0\n' "$opt" >>"$result"
        cd mixed
        status=0
        "${run[@]}" "-$opt" -c NativeTypeLambdaEta.hs >mixed.log 2>&1 || status=$?
        printf '%s\tmixed\t%s\n' "$opt" "$status" >>"$result"
        if [[ "$mode" == baseline ]]; then
            test "$status" = 1
            rg -q '^NativeTypeLambdaEta.hs:[0-9]+:[0-9]+: error: \[GHC-83865\]' mixed.log
        else
            test "$status" = 0
            "${run[@]}" --show-iface NativeTypeLambdaEta.hi >mixed.iface
            if [[ "$opt" == O2 ]]; then
                grep -Fq scoped-lambda-eta-v1 mixed.iface
            fi
            mkdir reader
            cp ./*.{hi,o} ../provider/*.o reader/
            (
                cd reader
                cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import NativeTypeLambdaEta (visible)
main :: IO ()
main = print visible
HS
                "${run[@]}" "-$opt" -c Main.hs
                "${run[@]}" "-$opt" Main.o NativeTypeLambdaEta.o NativeTypeLambdaEtaLib.o -o eta
                printf '42\n' | cmp - <(./eta)
            )
        fi

        cp "$root/NativeTypeLambdaEtaReject.hs" .
        for reject in WRONG_CAPTURE REORDERED CONSTANT INVERSE MATCHABLE FALLBACK; do
            status=0
            "${run[@]}" "-$opt" "-D$reject" -c NativeTypeLambdaEtaReject.hs >"$reject.log" 2>&1 || status=$?
            printf '%s\t%s\t%s\n' "$opt" "$reject" "$status" >>"$result"
            test "$status" = 1
            rg -q '^NativeTypeLambdaEtaReject.hs:[0-9]+:[0-9]+: error: \[GHC-83865\]' "$reject.log"
        done
    )
done
