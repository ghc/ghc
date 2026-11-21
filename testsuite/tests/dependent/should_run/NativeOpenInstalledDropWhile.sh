#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client,shapes}
cd "$out/provider"
stripped=(-fomit-interface-pragmas -fno-write-if-simplified-core)
files=(Prelude{MapLength{Size,Source},AppendLengthSource,Filter{Source,LengthProof,Runtime,AppendProof},TakeWhile,DropWhileSource}.hs)
for file in "${files[@]}"; do cp "$src/$file" .; done
"${ghc[@]}" -O2 "${stripped[@]}" -c "${files[@]}"

cd "$out/client"
cp ../provider/*.hi "$src/PreludeDropWhileProof.hs" .
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import Data.Type.Equality ((:~:)(Refl))
import PreludeDropWhileProof (prefixAndSuffix)
import PreludeFilterAppendProof (FilterDecisions(..))
import PreludeFilterRuntime (BooleanResult(..))
import PreludeFilterSource (Negation)

passed :: Bool
passed = case prefixAndSuffix decisions of Refl -> True
  where
    decisions :: FilterDecisions Negation '[ 'False, 'True, 'False]
    decisions = Decide IsTrue (Decide IsFalse (error "proof inspected after rejection"))

main :: IO ()
main = do
    let input = False : True : error "rejection forced the suffix tail"
        suffix = dropWhile not input
        rejected = dropWhile (const False) (undefined : undefined :: [Bool])
        needsTail = dropWhile (const True) (undefined : error "accepted tail" :: [Bool])
    demanded <- try (evaluate needsTail)
    print ( passed, takeWhile not input, take 1 suffix, hasCons rejected
          , null (dropWhile (error "empty input forced callback") ([] :: [Bool]))
          , null (dropWhile (const True) [undefined :: Bool])
          , case demanded of Left (ErrorCall "accepted tail") -> True; _ -> False )
  where
    hasCons (_ : _) = True
    hasCons [] = False
HS
for level in -O0 -O2; do
    "${ghc[@]}" "$level" -fforce-recomp -c PreludeDropWhileProof.hs Main.hs
    "${ghc[@]}" Main.o PreludeDropWhileProof.o ../provider/*.o -o run
    [[ $(./run) == '(True,[False],[True],True,True,True,True)' ]]
done

sed 's/(Decide IsFalse _)/(Decide _ _)/' PreludeDropWhileProof.hs >WrongDecision.hs
line=$(awk '/^prefixAndSuffix \(Decide _ _\)/ { print NR }' WrongDecision.hs)
if "${ghc[@]}" -O0 -c WrongDecision.hs >wrong-decision.log 2>&1 ||
   ! grep -Fq "WrongDecision.hs:$line:" wrong-decision.log ||
   ! grep -Fq '[GHC-05617]' wrong-decision.log ||
   ! grep -Fq '#guard:dropWhile (p x)' wrong-decision.log; then
    cat wrong-decision.log >&2
    exit 1
fi

# Check the branch shapes independently of the installed definition names.
cd "$out/shapes"
cat >Walk.hs <<'HS'
{-# OPTIONS_GHC -fwrite-source-equations #-}
module Walk (walk) where

walk :: (a -> Bool) -> [a] -> [a]
walk _ [] = []
walk p input@(x : xs)
    | p x = walk p xs
    | otherwise = input
HS
sed -e 's/= walk p xs$/= x : walk p xs/' -e 's/otherwise = input/otherwise = walk p xs/' Walk.hs >Keep.hs
sed -e 's/= walk p xs$/= x : walk p xs/' -e 's/otherwise = input/otherwise = []/' Walk.hs >Stop.hs
sed 's/p x =/p (error "other head") =/' Walk.hs >OtherHead.hs
sed 's/walk p xs$/walk p (x : xs)/' Walk.hs >RebuiltInput.hs
sed 's/walk p xs$/walk (not . p) xs/' Walk.hs >OtherPredicate.hs
sed 's/otherwise = input/otherwise = xs `seq` input/' Walk.hs >StrictSuffix.hs
for shape in Walk Keep Stop OtherHead RebuiltInput OtherPredicate StrictSuffix; do
    "${ghc[@]}" -O2 "${stripped[@]}" -fforce-recomp -c "$shape.hs" -ohi "$shape.hi"
    "${ghc[@]}" --show-iface "$shape.hi" >"$shape.iface"
done
for shape in Walk Keep Stop; do
    grep -Fq 'FinalCoreSourceTable T:walk' "$shape.iface"
    grep -Fq 'T:#guard:walk' "$shape.iface"
done
for shape in OtherHead RebuiltInput OtherPredicate StrictSuffix; do
    if grep -Eq 'T:(#guard:)?walk' "$shape.iface"; then
        cat "$shape.iface" >&2
        exit 1
    fi
done

# A rule can substitute a different, otherwise admissible result.
cp Walk.hs Ruled.hs
cat >>Ruled.hs <<'HS'

{-# NOINLINE [1] walk #-}
{-# RULES "walk/stop" forall p xs. walk p xs = [] #-}
HS
"${ghc[@]}" -O2 "${stripped[@]}" -fno-enable-rewrite-rules -c Ruled.hs -ohi Ruled.hi
"${ghc[@]}" --show-iface Ruled.hi >without-rules.iface
grep -Fq 'FinalCoreSourceTable T:walk' without-rules.iface
"${ghc[@]}" -O2 "${stripped[@]}" -fforce-recomp -ddump-rule-firings -c Ruled.hs -ohi Ruled.hi >fired.log
grep -Fq 'Rule fired: walk/stop' fired.log
"${ghc[@]}" --show-iface Ruled.hi >with-rules.iface
if grep -Eq 'T:(#guard:)?walk' with-rules.iface; then
    cat with-rules.iface >&2
    exit 1
fi
