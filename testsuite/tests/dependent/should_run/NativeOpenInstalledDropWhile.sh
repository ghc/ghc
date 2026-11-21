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
files=(Prelude{MapLength{Size,Source},AppendLengthSource,Filter{Source,LengthProof,Runtime,AppendProof},TakeWhile,DropWhileSource,SpanSource}.hs)
files+=(NativeOpenSpanShapes.hs)
for file in "${files[@]}"; do cp "$src/$file" .; done
sed -e 's/PreludeSpanSource/SpanShapeSource/' \
    -e '/^import GHC.Exts/a import NativeOpenSpanShapes (lazySpan)' \
    -e 's/= span p xs$/= lazySpan p xs/' PreludeSpanSource.hs >SpanShapeSource.hs
files+=(SpanShapeSource.hs)
"${ghc[@]}" -O2 "${stripped[@]}" -c "${files[@]}"
"${ghc[@]}" --show-iface NativeOpenSpanShapes.hi >SpanShapes.iface
grep -Fxq '  FinalCoreSourceTable T:lazySpan' SpanShapes.iface
if grep -Fq 'T:strictSpan' SpanShapes.iface; then exit 1; fi

cd "$out/client"
cp ../provider/*.hi "$src"/Prelude{DropWhileProof,SpanProof}.hs .
sed -e 's/PreludeSpanProof/SpanShapeProof/' -e 's/PreludeSpanSource/SpanShapeSource/' \
    PreludeSpanProof.hs >SpanShapeProof.hs
cat >ClosedSpan.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module ClosedSpan where

import Data.Type.Equality ((:~:)(Refl))
import PreludeDropWhileSource (Rejoined)

type Split = span not '[ 'False, 'True, 'False]

pieces :: Split :~: '( '[ 'False], '[ 'True, 'False])
pieces = Refl

reassembled :: Rejoined Split :~: '[ 'False, 'True, 'False]
reassembled = Refl
HS
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import Data.Type.Equality ((:~:)(Refl))
import PreludeDropWhileProof (rejoinPair)
import PreludeFilterAppendProof (FilterDecisions(..))
import PreludeFilterRuntime (BooleanResult(..))
import PreludeFilterSource (Negation)
import PreludeSpanProof (rejoinSpan)
import qualified NativeOpenSpanShapes as SpanShapes

passed :: Bool
passed = case (rejoinPair decisions, rejoinSpan decisions) of (Refl, Refl) -> True
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
    strict <- try (evaluate (firstAccepted SpanShapes.strictSpan))
    spine <- try (evaluate (span id (error "span spine" :: [Bool])))
    predicate <- try (evaluate (span (error "span predicate") [True]))
    let (prefix, rest) = span not input
        finite = [False, True, False]
        (before, after) = span not finite
        unknown = error "separate prefix and suffix spine" :: [Bool]
    print ( firstAccepted span, firstAccepted SpanShapes.lazySpan
          , take 2 (prefix ++ rest), before ++ after == finite
          , raised "span tail" strict, raised "span spine" spine
          , raised "span predicate" predicate
          , (takeWhile id unknown, dropWhile id unknown) `seq` True )
  where
    hasCons (_ : _) = True
    hasCons [] = False

    firstAccepted split = case fst (split id (True : error "span tail")) of
        True : _ -> True
        _ -> False

    raised message (Left (ErrorCall actual)) = actual == message
    raised _ _ = False
HS
for level in -O0 -O2; do
    "${ghc[@]}" "$level" -fforce-recomp -c PreludeDropWhileProof.hs PreludeSpanProof.hs \
        SpanShapeProof.hs ClosedSpan.hs Main.hs
    "${ghc[@]}" Main.o PreludeDropWhileProof.o PreludeSpanProof.o ../provider/*.o -o run
    ./run >actual
    cat >expected <<'OUT'
(True,[False],[True],True,True,True,True)
(True,True,[False,True],True,True,True,True,True)
OUT
    cmp expected actual
done

cat >StrictSpanSource.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, Modifiers #-}
module StrictSpanSource where
import GHC.Exts (U)
import NativeOpenSpanShapes (strictSpan)

type Split :: (a %U -> Bool) -> [a] -> ([a], [a])
type Split p xs = strictSpan p xs
HS
if "${ghc[@]}" -O0 -c StrictSpanSource.hs >strict-span.log 2>&1 ||
   ! grep -Eq '^StrictSpanSource.hs:[0-9]+:[0-9]+: error: \[GHC-70565\]' strict-span.log ||
   ! grep -Fq 'type synonym declaration for ‘Split’' strict-span.log; then
    cat strict-span.log >&2
    exit 1
fi

sed 's/(Decide IsFalse _)/(Decide _ _)/' PreludeSpanProof.hs >WrongSpanDecision.hs
line=$(awk '/^rejoinSpan \(Decide _ _\)/ { print NR }' WrongSpanDecision.hs)
if "${ghc[@]}" -O0 -c WrongSpanDecision.hs >wrong-span-decision.log 2>&1 ||
   ! grep -Fq "WrongSpanDecision.hs:$line:" wrong-span-decision.log ||
   ! grep -Fq '[GHC-05617]' wrong-span-decision.log ||
   ! grep -Fq '#guard:span (p x)' wrong-span-decision.log; then
    cat wrong-span-decision.log >&2
    exit 1
fi

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
