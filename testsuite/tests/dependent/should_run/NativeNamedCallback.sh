#!/usr/bin/env bash
set -euo pipefail

src=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans)
strip=(-fomit-interface-pragmas -fno-write-if-simplified-core)

for opt in 0 2; do
    flags=("-O$opt" "${strip[@]}")
    mkdir -p "$work/$opt"/{provider,reader}
    cd "$work/$opt/provider"
    cp "$src"/{PreludeMapLengthSize,PreludeZipMapRuntime,PreludeZipRuntime}.hs .

    cat >OtherSizes.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module OtherSizes (otherSize) where

import PreludeMapLengthSize (Size(..))

otherSize :: [a] -> Size
otherSize [] = Succ Zero
otherSize (_ : xs) = Succ (otherSize xs)
HS

    cat >MeasuredZip.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module MeasuredZip (measuredZip, runMeasure) where

import Data.Type.Equality ((:~:)(Refl))
import PreludeMapLengthSize (Size, minSize, size)
import PreludeZipRuntime (lengthZip)

measuredZip :: forall a b. foreach (measure :: [a] -> Size)
            -> foreach (xs :: [a]) -> foreach (ys :: [b])
            -> size xs :~: measure xs
            -> size (zip xs ys) :~: minSize (measure xs) (size ys)
measuredZip _ xs ys known =
    case lengthZip xs ys of Refl -> case known of Refl -> Refl

runMeasure :: forall a. foreach (measure :: [a] -> Size) -> foreach (xs :: [a])
           -> (Size, measure xs :~: measure xs)
runMeasure measure xs = (measure xs, Refl)
HS

    cat >ChangeSize.hs <<'HS'
{-# LANGUAGE ScopedTypeVariables, NoExperimentalDependentTypes #-}
module ChangeSize () where
import qualified PreludeMapLengthSize as A

{-# RULES "size/changed" forall (xs :: [Bool]). A.size xs = A.Zero #-}
HS

    for mod in PreludeMapLengthSize PreludeZipMapRuntime PreludeZipRuntime OtherSizes MeasuredZip; do
        "${ghc[@]}" "${flags[@]}" -c "$mod.hs"
        cp "$mod".{hi,o} ../reader/
    done
    "${ghc[@]}" "-O$opt" -fno-omit-interface-pragmas -c ChangeSize.hs
    cp ChangeSize.hi ../reader/

    cd ../reader
    cat >Main.hs <<'HS'
{-# LANGUAGE CPP, GADTs, NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import Data.Type.Equality ((:~:)(Refl))
import MeasuredZip (measuredZip, runMeasure)
import OtherSizes (otherSize)
import qualified PreludeMapLengthSize as A
import qualified PreludeMapLengthSize as B
#ifdef CHANGE_SIZE
import ChangeSize ()
#endif

uncheckedSize :: [a] -> A.Size
uncheckedSize _ = A.Zero

check :: [Bool] -> [Int] -> Bool
check xs ys =
#ifdef WRONG_FUNCTION
    case measuredZip otherSize xs ys Refl of Refl -> True
#elif defined(UNCHECKED)
    case measuredZip uncheckedSize xs ys Refl of Refl -> True
#else
    case measuredZip A.size xs ys Refl of
        Refl -> case measuredZip (B.size @Bool) xs ys Refl of Refl -> True
#endif

main :: IO ()
main = do
    let element = error "element" :: Bool
        tailBottom = error "unused tail" :: [Bool]
        asInt A.Zero = 0 :: Int
        asInt (A.Succ n) = 1 + asInt n
    print (check [] (error "unused right"), check [element] [0])
    print (asInt (fst (runMeasure A.size [element, element])),
           asInt (fst (runMeasure otherSize [element, element])))
    print (case fst (runMeasure A.size (element : tailBottom)) of
             A.Zero -> False
             A.Succ _ -> True)
    result <- try @ErrorCall (evaluate (check [element] (error "required right")))
    case result of
        Left err | "required right" `isPrefixOf` displayException err -> pure ()
        _ -> fail "expected the right spine to be demanded"
HS

    "${ghc[@]}" "${flags[@]}" -c Main.hs
    "${ghc[@]}" ./*.o -o run
    ./run > result
    printf '(True,True)\n(2,3)\nTrue\n' > expected
    diff -u expected result

    reject() {
        local expected=$1
        shift
        if "${ghc[@]}" "${flags[@]}" -fforce-recomp -c Main.hs "$@" >reject.log 2>&1 ||
           ! grep -Fq "$expected" reject.log; then
            cat reject.log >&2
            exit 1
        fi
    }
    reject '[GHC-83865]' -DWRONG_FUNCTION
    grep -Fq 'otherSize xs' reject.log
    reject 'No checked source equations are available for' -DUNCHECKED
    reject 'A rewrite rule can change the result of' -DCHANGE_SIZE \
        -fno-ignore-interface-pragmas -fenable-rewrite-rules
done
