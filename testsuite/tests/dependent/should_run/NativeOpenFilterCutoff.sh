#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
src=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
mkdir "$work"/{provider,client}
cd "$work/provider"
cp "$src"/Prelude{MapLength{Size,Source},Filter{Source,LengthProof,Runtime,Cutoff}}.hs .
"${ghc[@]}" -O1 -c PreludeMapLength{Size,Source}.hs PreludeFilter{Source,LengthProof}.hs

cat >"$work/client/Main.hs" <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall(..), evaluate, try)
import Data.IORef (newIORef, readIORef)
import PreludeFilterCutoff (CutoffBound(..), packCutoff, showCutoffBound)
import PreludeFilterLengthProof (AtMost(..))
import PreludeMapLengthSize (Size(..))
import System.Environment (getArgs)

first :: CutoffBound -> String
first (CutoffBound _ _ bound) = case bound of
    ZeroBound -> "empty"
    BothSucc _ -> "keep"
    RightSucc _ -> "skip"

sizeFromInt :: Int -> Size
sizeFromInt n
    | n <= 0 = Zero
    | otherwise = Succ (sizeFromInt (n - 1))

main :: IO ()
main = do
    [cutoffInput, rowInput] <- getArgs
    cutoff <- newIORef (sizeFromInt (read cutoffInput)) >>= readIORef
    rows <- newIORef (read rowInput) >>= readIORef
    putStrLn (showCutoffBound (packCutoff cutoff rows))
    putStrLn (first (packCutoff (error "empty input forced cutoff") []))
    putStrLn (first (packCutoff (error "empty row forced cutoff") ([] : error "outer tail")))
    putStrLn (first (packCutoff Zero ((error "element" : error "inner tail") : error "outer tail")))
    putStrLn (first (packCutoff (Succ Zero) ([error "element"] : error "outer tail")))
    putStrLn (showCutoffBound (packCutoff (Succ Zero) [repeat False]))
    result <- try (evaluate (first (packCutoff (error "nonempty row needs cutoff") [[False]])))
    print (case result of Left (ErrorCall "nonempty row needs cutoff") -> True; _ -> False)
HS

cat >"$work/client/WrongCapture.hs" <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeApplications #-}
module WrongCapture where
import PreludeFilterCutoff (decideRow)
import PreludeFilterLengthProof (AtMost)
import PreludeFilterRuntime (runtimeFilterBoundFor)
import PreludeFilterSource (FilterWith)
import PreludeMapLengthSource (Count)

wrong :: foreach (f :: [Bool] -> Bool) -> foreach (g :: [Bool] -> Bool)
      -> foreach (rows :: [[Bool]]) -> AtMost (Count (FilterWith f rows)) (Count rows)
wrong f g rows = runtimeFilterBoundFor @[Bool] @g (decideRow g) rows
HS

expected=(
  '(BothSucc (RightSucc (RightSucc ZeroBound)),[Zero],Succ (Succ (Succ Zero)))'
  '(BothSucc (BothSucc (RightSucc ZeroBound)),[Zero,Succ Zero],Succ (Succ (Succ Zero)))'
  '(BothSucc (BothSucc (BothSucc ZeroBound)),[Zero,Succ Zero,Succ (Succ Zero)],Succ (Succ (Succ Zero)))'
)
demand=$'empty\nkeep\nskip\nkeep\n(RightSucc ZeroBound,[],Succ Zero)\nTrue'
for level in 0 1 2; do
    cd "$work/provider"
    "${ghc[@]}" "-O$level" -fforce-recomp -c PreludeFilter{Runtime,Cutoff}.hs
    cd "$work/client"
    cp ../provider/*.hi .
    "${ghc[@]}" "-O$level" -fforce-recomp -c Main.hs
    "${ghc[@]}" Main.o ../provider/*.o -o cutoff-run
    for cutoff in 0 1 2; do
        [[ $(./cutoff-run "$cutoff" '[[],[False],[True,False]]') == "${expected[cutoff]}"$'\n'"$demand" ]]
    done
done

if "${ghc[@]}" -O1 -c WrongCapture.hs >wrong.log 2>&1 ||
   ! grep -Fq 'WrongCapture.hs:11:' wrong.log ||
   ! grep -Fq '[GHC-83865]' wrong.log ||
   ! grep -Fq 'size (filter g rows)' wrong.log ||
   ! grep -Fq 'size (filter f rows)' wrong.log; then
    cat wrong.log >&2
    exit 1
fi
