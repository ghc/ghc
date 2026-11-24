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
    if [[ "$status" != 1 ]] || ! grep -Eq "\\[GHC-($code)\\]" negative.log; then
        cat negative.log >&2
        return 1
    fi
}

for opt in -O0 -O2; do
    mkdir -p "$work/$opt"/{provider,reader}
    cd "$work/$opt/provider"
    cp "$src/NativeLocalListAccumulator.hs" .
    producer=("$opt" -fomit-interface-pragmas -fno-write-if-simplified-core
              -c NativeLocalListAccumulator.hs)
    "${ghc[@]}" "${producer[@]}"
    "${ghc[@]}" --show-iface NativeLocalListAccumulator.hi >provider.iface
    grep -Fxq '  SourceTable T:shunt' provider.iface
    cp NativeLocalListAccumulator.{hi,o} ../reader/
    reject 83865 "${producer[@]}" -DFALSE_EQ -fforce-recomp
    reject 68947 "${producer[@]}" -DPARTIAL -fforce-recomp
    reject 68947 "${producer[@]}" -DNONDECREASING -fforce-recomp
    reject 68947 "${producer[@]}" -DFORCED -fforce-recomp
    reject 68947 "${producer[@]}" -XStrict -fforce-recomp
    "${ghc[@]}" "${producer[@]}" -fno-code -fwrite-interface -fforce-recomp
    if [[ "$opt" == -O2 ]]; then
        reject 83605 "${producer[@]}" -DRULE -fforce-recomp
        "${ghc[@]}" "${producer[@]}" -DRULE -fno-enable-rewrite-rules -fforce-recomp
    fi
    cd ../reader
    test ! -e NativeLocalListAccumulator.hs
    cat >Main.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications, TypeOperators #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import NativeLocalListAccumulator (shunt)

open :: foreach (xs :: [Bool]) -> foreach (ys :: [Bool])
#ifdef FALSE_EQ
     -> shunt ('True ': xs) ys :~: shunt xs ('False ': ys)
#else
     -> shunt ('True ': xs) ys :~: shunt xs ('True ': ys)
#endif
open _ _ = Refl

onEmpty :: foreach (ys :: [Bool]) -> shunt '[] ys :~: ys
onEmpty _ = Refl

{-# NOINLINE spine #-}
spine :: [Bool] -> Bool
spine [] = False
spine (_ : _) = True

main :: IO ()
main = do
    print (open [] [], onEmpty [])
    print (shunt [True, False] [True])
    print (spine (shunt [error "head forced"] (error "accumulator forced")))
    demanded <- try @ErrorCall (evaluate (spine (shunt (True : error "input tail") [])))
    case demanded of
        Left exception | "input tail" `isInfixOf` displayException exception ->
            putStrLn "demanded input tail"
        _ -> fail "skipped input tail"
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o NativeLocalListAccumulator.o -o check
    ./check >actual
    printf '%s\n' '(Refl,Refl)' '[False,True,True]' True 'demanded input tail' | diff -u - actual
    reject 83865 "$opt" -DFALSE_EQ -fforce-recomp -c Main.hs
done

for opt in -O0 -O2; do
    mkdir -p "$work/reverse/$opt"/{provider,client}
    cd "$work/reverse/$opt/provider"
    cat >ReverseSource.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module ReverseSource (app, rev) where

app :: [a] -> [a] -> [a]
app [] ys = ys
app (x:xs) ys = x : app xs ys

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = app (rev xs) [x]
HS
    "${ghc[@]}" "$opt" -fno-enable-rewrite-rules -fwrite-source-equations \
        -fno-code -fwrite-interface -c ReverseSource.hs
    test ! -e ReverseSource.o
    "${ghc[@]}" --show-iface ReverseSource.hi >reverse.iface
    grep -Fxq '  SourceTable T:rev' reverse.iface
    cp ReverseSource.hi ../client/
    cd ../client
    test ! -e ReverseSource.hs
    cat >ReverseProof.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module ReverseProof where

import Data.Type.Equality ((:~:)(Refl))
import ReverseSource (app, rev)

#ifdef RULE
{-# RULES "skip append" forall xs. app [] xs = [] #-}
#endif

step :: foreach (x :: Bool) -> foreach (xs :: [Bool])
#ifdef FALSE_REV
     -> rev (x ': xs) :~: app (rev xs) '[ 'False]
#else
     -> rev (x ': xs) :~: app (rev xs) '[x]
#endif
step _ _ = Refl
HS
    proof=("$opt" -fno-code -fwrite-interface -c ReverseProof.hs)
    "${ghc[@]}" "${proof[@]}"
    reject '83865|25897' "${proof[@]}" -DFALSE_REV -fforce-recomp
    if [[ "$opt" == -O2 ]]; then
        reject 83605 "${proof[@]}" -DRULE -fforce-recomp
    fi
done
