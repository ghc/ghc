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
            grep -Eq '^NativeTypeLambdaEta.hs:[0-9]+:[0-9]+: error: \[GHC-83865\]' mixed.log
        else
            test "$status" = 0
            "${run[@]}" --show-iface NativeTypeLambdaEta.hi >mixed.iface
            if [[ "$opt" == O2 ]]; then
                grep -Fq scoped-lambda-eta-v1 mixed.iface
                grep -Fq ScopedLambdaEta mixed.iface
            fi
            mkdir reader
            cp ./*.{hi,o} ../provider/*.o reader/
            (
                cd reader
                cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaEta (overRoute, partialStage, reverseMap, routed, visible)

fromImportedProof :: forall (xs :: [Type]). Proxy xs -> Int
fromImportedProof witness =
  case reverseMap @Int @xs witness of
    Refl -> visible

fromRoute :: forall (flag :: Bool). Proxy flag -> (Int, Int)
fromRoute _ =
  case partialStage @Int Proxy of
    Refl -> case overRoute @Int @flag Proxy of
      Refl -> (fromImportedProof (Proxy @'[Bool]), routed)

main :: IO ()
main = print (fromRoute (Proxy @'True))
HS
                test ! -e NativeTypeLambdaEta.hs
                "${run[@]}" "-$opt" -ddump-inlinings -dsuppress-uniques -c Main.hs >reader.log 2>&1
                if [[ "$opt" == O2 ]]; then
                    for proof in reverseMap partialStage overRoute; do
                        grep -Eq "Inlining done: (NativeTypeLambdaEta\.)?$proof" reader.log
                    done
                fi
                "${run[@]}" "-$opt" Main.o NativeTypeLambdaEta.o NativeTypeLambdaEtaLib.o -o eta
                printf '(42,42)\n' | cmp - <(./eta)
            )
        fi

        cp "$root/NativeTypeLambdaEtaReject.hs" .
        for reject in WRONG_CAPTURE WRONG_SPINE_CAPTURE REORDERED CONSTANT INVERSE MATCHABLE FALLBACK; do
            status=0
            "${run[@]}" "-$opt" "-D$reject" -c NativeTypeLambdaEtaReject.hs >"$reject.log" 2>&1 || status=$?
            printf '%s\t%s\t%s\n' "$opt" "$reject" "$status" >>"$result"
            test "$status" = 1
            code=83865
            if [[ "$reject" == INVERSE ]]; then code=25897; fi
            grep -Eq "^NativeTypeLambdaEtaReject.hs:[0-9]+:[0-9]+: error: \[GHC-$code\]" "$reject.log"
        done
    )
done
