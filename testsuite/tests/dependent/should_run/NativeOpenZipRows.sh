#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -O0 -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans -Werror=inaccessible-code -i -i.)
strip=(-fomit-interface-pragmas -fno-write-if-simplified-core)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,reader,rule,nocode,nocode-reader,counting}
mkdir "$out"/{specialized-provider,specialized-reader}

reject() {
    local expected=$1
    shift
    if "${ghc[@]}" -c "$@" >reject.log 2>&1 || ! grep -Eq "$expected" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

cd "$out/provider"
cp "$src/"{PackedVecLibrary,ZipLengthSource}.hs .
"${ghc[@]}" "${strip[@]}" -c PackedVecLibrary.hs
"${ghc[@]}" -O2 "${strip[@]}" -c ZipLengthSource.hs
"${ghc[@]}" --show-iface ZipLengthSource.hi >provider.iface
grep -Fq 'FinalCoreSourceTable T:zipLength' provider.iface

cat >ZipVariants.hs <<'HS'
module ZipVariants
    (zipConcrete, countRight, countLeft, countEither,
     loopLength, swapCycle, crossFields, growRight, growBoth) where
import PackedVecLibrary (Nat(..))

zipConcrete :: [Bool] -> [Char] -> Nat
zipConcrete [] _ = Z
zipConcrete (_ : _) [] = Z
zipConcrete (_ : xs) (_ : ys) = S (zipConcrete xs ys)

countRight :: forall a b. [a] -> [b] -> Nat
countRight [] _ = Z
countRight (_ : _) [] = Z
countRight left@(_ : _) (_ : ys) = S (countRight left ys)

countLeft :: forall a b. [a] -> [b] -> Nat
countLeft [] _ = Z
countLeft (_ : _) [] = Z
countLeft (_ : xs) right@(_ : _) = S (countLeft xs right)

countEither :: [Bool] -> [Bool] -> Nat
countEither [] _ = Z
countEither (_ : _) [] = Z
countEither (_ : xs) right@(False : _) = S (countEither xs right)
countEither left@(_ : _) (True : ys) = S (countEither left ys)

loopLength :: [Bool] -> [Char] -> Nat
loopLength [] _ = Z
loopLength (_ : _) [] = Z
loopLength left@(_ : _) right@(_ : _) = S (loopLength left right)

swapCycle :: [Bool] -> [Bool] -> Nat
swapCycle [] _ = Z
swapCycle (_ : _) [] = Z
swapCycle left@(_ : _) right@(_ : _) = S (swapCycle right left)

crossFields :: [Bool] -> [Bool] -> Nat
crossFields [] _ = Z
crossFields (_ : _) [] = Z
crossFields (_ : xs) (False : ys) = S (crossFields xs ys)
crossFields (_ : xs) (True : ys) = S (crossFields ys xs)

growRight :: [Bool] -> [Char] -> Nat
growRight [] _ = Z
growRight (_ : _) [] = Z
growRight (_ : xs) (y : ys) = S (growRight xs (y : y : ys))

growBoth :: [Bool] -> [Char] -> Nat
growBoth [] _ = Z
growBoth (_ : _) [] = Z
growBoth (x : xs) (y : ys) = S (growBoth (x : x : xs) (y : y : ys))
HS
"${ghc[@]}" "${strip[@]}" -c ZipVariants.hs
"${ghc[@]}" --show-iface ZipVariants.hi >variants.iface
for name in zipConcrete countRight countLeft countEither; do
    grep -Fq "FinalCoreSourceTable T:$name" variants.iface
done
for name in loopLength swapCycle crossFields growRight growBoth; do
    if grep -Fq "FinalCoreSourceTable T:$name" variants.iface; then
        echo "Exported an unchecked equation for $name." >&2
        exit 1
    fi
done

cd "$out/reader"
cp ../provider/{PackedVecLibrary,ZipLengthSource,ZipVariants}.hi "$src/ZipRows.hs" .
cat >Laws.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, GADTs #-}
module Laws where
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..))
import ZipLengthSource (zipLength)
import ZipVariants (countEither, countLeft, countRight, zipConcrete)

emptyLeft :: forall (ys :: [Bool]). zipLength ('[] :: [Char]) ys :~: 'Z
emptyLeft = Refl

emptyRight :: forall (x :: Char) (xs :: [Char]). zipLength (x ': xs) ('[] :: [Bool]) :~: 'Z
emptyRight = Refl

bothCons :: forall (x :: Char) (xs :: [Char]) (y :: Bool) (ys :: [Bool]).
            zipLength (x ': xs) (y ': ys) :~: 'S (zipLength xs ys)
bothCons = Refl

concrete :: forall (ys :: [Char]). zipConcrete '[] ys :~: 'Z
concrete = Refl

rightStep :: forall (x :: Char) (xs :: [Char]) (y :: Bool) (ys :: [Bool]).
             countRight (x ': xs) (y ': ys) :~: 'S (countRight (x ': xs) ys)
rightStep = Refl

leftStep :: forall (x :: Char) (xs :: [Char]) (y :: Bool) (ys :: [Bool]).
            countLeft (x ': xs) (y ': ys) :~: 'S (countLeft xs (y ': ys))
leftStep = Refl

changeLeft :: forall (x :: Bool) (xs :: [Bool]) (ys :: [Bool]).
              countEither (x ': xs) ('False ': ys) :~: 'S (countEither xs ('False ': ys))
changeLeft = Refl

changeRight :: forall (x :: Bool) (xs :: [Bool]) (ys :: [Bool]).
               countEither (x ': xs) ('True ': ys) :~: 'S (countEither (x ': xs) ys)
changeRight = Refl
HS
"${ghc[@]}" -fenable-rewrite-rules -c Laws.hs
"${ghc[@]}" -Werror=incomplete-patterns -Werror=overlapping-patterns -c ZipRows.hs
cp "$src/OrdinaryZipRows.hs" .
"${ghc[@]}" -c OrdinaryZipRows.hs

cat >LaterFallback.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module LaterFallback (which, choiceNumber) where

newtype Choice (xs :: [Char]) (ys :: [Bool]) = Choice Int

which :: foreach (xs :: [Char]) -> foreach (ys :: [Bool]) -> Choice xs ys
which [] _ = Choice 0
which (_ : _) [] = Choice 1
which (_ : _) ys = Choice (if null ys then 9 else 2)

choiceNumber :: Choice xs ys -> Int
choiceNumber (Choice n) = n
HS
"${ghc[@]}" -Werror=incomplete-patterns -Werror=overlapping-patterns -c LaterFallback.hs

cat >Main.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Main where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import Data.Type.Equality ((:~:)(Refl))
import LaterFallback (which, choiceNumber)
import OrdinaryZipRows (zipReport)
import PackedVecLibrary (natToInt, toList)
import ZipRows (zipMinimum, zipRows)
import ZipVariants (countEither, countLeft, countRight)

minimumHolds :: [a] -> [b] -> Bool
minimumHolds xs ys = case zipMinimum xs ys of Refl -> True

main :: IO ()
main = do
    print (toList (zipRows "ab" [True]), toList (zipRows "a" [True, False]))
    let infinite = repeat 'a'
        badFirst = error "first spine" :: String
        badSecond = error "second spine" :: [Bool]
        badChar = error "first element" :: Char
        badBool = error "second element" :: Bool
        emptyB = [] :: [Bool]
        leftTail = 'a' : error "first tail"
        rightTail = True : error "second tail"
    print (toList (zipRows infinite [True, False]))
    print (null (toList (zipRows "" badSecond)))
    print (length (toList (zipRows [badChar] [badBool])))
    print (take 1 (toList (zipRows leftTail rightTail)), null (toList (zipRows leftTail emptyB)))
    first <- try @ErrorCall (evaluate (null (toList (zipRows badFirst emptyB))))
    print (either (isPrefixOf "first spine" . displayException) (const False) first)
    print (choiceNumber (which "" badSecond), choiceNumber (which "a" emptyB), choiceNumber (which "a" [True]))
    second <- try @ErrorCall (evaluate (choiceNumber (which "a" badSecond)))
    print (either (isPrefixOf "second spine" . displayException) (const False) second)
    print (minimumHolds "" badSecond, minimumHolds [badChar] [badBool],
           minimumHolds infinite [True, False])
    print (natToInt (countRight "" badSecond),
           natToInt (countRight (badChar : error "first tail") [True, False]),
           natToInt (countLeft "ab" rightTail), natToInt (countEither [False, True] [True, False]))
    rightFirst <- try @ErrorCall (evaluate (natToInt (countRight badFirst emptyB)))
    print (either (isPrefixOf "first spine" . displayException) (const False) rightFirst)
    putStrLn (zipReport "abc" [True, False])
    putStrLn (zipReport "" badSecond)
HS
"${ghc[@]}" -c Main.hs
"${ghc[@]}" Main.o OrdinaryZipRows.o ZipRows.o LaterFallback.o \
    ../provider/{PackedVecLibrary,ZipLengthSource,ZipVariants}.o -o run
./run >actual
diff -u - actual <<'OUT'
([('a',True)],[('a',True)])
[('a',True),('a',False)]
True
1
([('a',True)],True)
True
(0,1,2)
True
(True,True,True)
(0,2,2,3)
True
([('a',True),('b',False)],Refl)
([],Refl)
OUT

awk '{sub(/^module ZipRows /,"module ShadowedRow "); print}
     /^zipRows \[\] _ = VNil$/ {print "zipRows [] [] = VNil"}' ZipRows.hs >ShadowedRow.hs
reject '^ShadowedRow.hs:13:12-13: error: \[GHC-83605\]' ShadowedRow.hs

cat >MissingPair.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module MissingPair where
data Phase = Empty | First | Last
data Packet (phase :: Phase) (value :: Bool) = Packet
bad :: foreach (phase :: Phase) -> foreach (value :: Bool) -> Packet phase value
bad Empty _ = Packet
bad First False = Packet
bad First True = Packet
bad Last False = Packet
HS
reject '^MissingPair.hs:\(6,1\)-\(9,23\): error: \[GHC-83605\]' MissingPair.hs
grep -Fq 'Missing equation for the retained constructor pair (Last, True).' reject.log

cat >WrongDemand.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, GADTs #-}
module WrongDemand where
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..))
import ZipLengthSource (zipLength)
wrong :: forall (xs :: [Char]). zipLength xs ('[] :: [Bool]) :~: 'Z
wrong = Refl
HS
reject '^WrongDemand.hs:7:9-12: error: \[GHC-83865\]' WrongDemand.hs
sed 's/WrongDemand/RightDemand/; s/ZipLengthSource/ZipVariants/; s/zipLength/countRight/g' \
    WrongDemand.hs >RightDemand.hs
reject '^RightDemand.hs:7:9-12: error: \[GHC-83865\]' RightDemand.hs

# Check the generated worker and its rule in the normal optimization pipeline.
cd "$out/specialized-provider"
cp ../provider/{PackedVecLibrary.hi,ZipVariants.hs} .
"${ghc[@]}" -O2 -ddump-rule-firings -c ZipVariants.hs >specialized.log
grep -Fq 'Rule fired: SC:countRight0 (ZipVariants)' specialized.log
"${ghc[@]}" --show-iface ZipVariants.hi >specialized.iface
grep -Fq '"SC:countRight0"' specialized.iface
grep -Fq 'FinalCoreSourceTable T:countRight' specialized.iface

cd "$out/specialized-reader"
cp ../specialized-provider/{PackedVecLibrary,ZipVariants}.hi ../reader/RightDemand.hs .
cat >Specialized.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, GADTs #-}
module Specialized where
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..))
import ZipVariants (countRight)
step :: forall (x :: Char) (xs :: [Char]) (y :: Bool) (ys :: [Bool]).
        countRight (x ': xs) (y ': ys) :~: 'S (countRight (x ': xs) ys)
step = Refl
HS
"${ghc[@]}" -fno-enable-rewrite-rules -c Specialized.hs
"${ghc[@]}" -O2 -fforce-recomp -c Specialized.hs
reject '^RightDemand.hs:7:9-12: error: \[GHC-83865\]' -fno-enable-rewrite-rules RightDemand.hs

cd "$out/specialized-provider"
"${ghc[@]}" -O2 "${strip[@]}" -fforce-recomp -c ZipVariants.hs
"${ghc[@]}" --show-iface ZipVariants.hi >stripped.iface
grep -Fq 'FinalCoreSourceTable T:countRight' stripped.iface
if grep -Fq '"SC:countRight0"' stripped.iface; then
    echo 'The stripped interface still exports the specialization rule.' >&2
    exit 1
fi
cd "$out/specialized-reader"
cp ../specialized-provider/ZipVariants.hi .
"${ghc[@]}" -fforce-recomp -fno-ignore-interface-pragmas -fenable-rewrite-rules -c Specialized.hs
"${ghc[@]}" ../reader/{Main,OrdinaryZipRows,ZipRows,LaterFallback}.o \
    ../provider/{PackedVecLibrary,ZipLengthSource}.o ../specialized-provider/ZipVariants.o -o run
./run >actual
diff -u ../reader/actual actual

# A rule with the same printed name and shape changes the result and demand.
cd "$out/specialized-provider"
cat >RuleSpoof.hs <<'HS'
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module RuleSpoof (handCount) where
import PackedVecLibrary (Nat(..))
import ZipVariants (countRight)

{-# OPAQUE wrongCount #-}
wrongCount :: a -> [a] -> [b] -> Nat
wrongCount x _ _ = x `seq` S (S (S Z))

handCount :: forall a b. [a] -> [b] -> Nat
handCount [] _ = Z
handCount (_ : _) [] = Z
handCount (x : xs) (_ : ys) = S (wrongCount x xs ys)

{-# RULES "SC:countRight0" forall x xs. countRight (x : xs) = wrongCount x xs #-}
HS
"${ghc[@]}" -O2 -c RuleSpoof.hs
cd "$out/specialized-reader"
cp ../specialized-provider/RuleSpoof.hi .
sed -e 's/module Specialized/module SpoofProof/' \
    -e '/^import ZipVariants /a import RuleSpoof ()' Specialized.hs >SpoofProof.hs
reject '^SpoofProof.hs:9:8-11: error: \[GHC-83605\]' -O2 SpoofProof.hs
sed -e 's/Specialized/HandProof/' -e 's/ZipVariants (countRight)/RuleSpoof (handCount)/' \
    -e 's/countRight/handCount/g' Specialized.hs >HandProof.hs
reject '^HandProof.hs:7:19-27: error: \[GHC-70565\]' -O2 HandProof.hs
cat >SpoofRun.hs <<'HS'
module Main where
import Control.Exception (ErrorCall, evaluate, try)
import PackedVecLibrary (natToInt)
import RuleSpoof (handCount)
import ZipVariants (countRight)
main :: IO ()
main = do
    print (natToInt (countRight (True : []) [True]), natToInt (handCount "a" [True]))
    demand <- try (evaluate (natToInt (countRight [error "head"] [True])))
    print (either (const True :: ErrorCall -> Bool) (const False) demand)
HS
"${ghc[@]}" -O2 -ddump-rule-firings -c SpoofRun.hs >spoof.log
grep -Fq 'Rule fired: SC:countRight0 (RuleSpoof)' spoof.log
"${ghc[@]}" SpoofRun.o ../specialized-provider/{ZipVariants,RuleSpoof}.o \
    ../provider/PackedVecLibrary.o -o spoof
./spoof >spoof.actual
diff -u - spoof.actual <<'OUT'
(3,4)
True
OUT

cd "$out/reader"

{ sed -n '1,4p' WrongDemand.hs | sed 's/WrongDemand/Nondecreasing/'
  cat <<'HS'
import ZipVariants (loopLength)
wrong :: forall (ys :: [Char]). loopLength '[] ys :~: 'Z
wrong = Refl
HS
} >Nondecreasing.hs
# The interpreter reports the open operand instead of the function name.
reject '^Nondecreasing.hs:6:(33-42|48-49): error: \[GHC-70565\]' Nondecreasing.hs

{ sed -n '1,5p' WrongDemand.hs | sed 's/WrongDemand/ConsumerRule/'
  cat <<'HS'
{-# RULES "change zip" forall xs ys. zipLength xs ys = S Z #-}
emptyLeft :: forall (ys :: [Bool]). zipLength ('[] :: [Char]) ys :~: 'Z
emptyLeft = Refl
HS
} >ConsumerRule.hs
reject '^ConsumerRule.hs:8:13-16: error: \[GHC-83605\]' -fenable-rewrite-rules ConsumerRule.hs

cd "$out/nocode-reader"
cp ../reader/{PackedVecLibrary,ZipLengthSource,ZipVariants}.hi ../reader/Laws.hs .
"${ghc[@]}" -fenable-rewrite-rules -fno-code -fwrite-interface -c Laws.hs
test -f Laws.hi && test ! -e Laws.o

# Keep the same case tree after this rule fires. Only the cons result changes.
cd "$out/rule"
cp ../provider/PackedVecLibrary.hi "$src/ZipLengthSource.hs" .
cat >>ZipLengthSource.hs <<'HS'

{-# NOINLINE zipLength #-}
{-# RULES "change recursive zip" forall xs ys. zipLength xs ys = Z #-}
HS
"${ghc[@]}" -O1 "${strip[@]}" -fno-full-laziness -ddump-rule-firings \
    -c ZipLengthSource.hs >rule.log
grep -Fq 'Rule fired: change recursive zip (ZipLengthSource)' rule.log
"${ghc[@]}" --show-iface ZipLengthSource.hi >rule.iface
if grep -Fq 'FinalCoreSourceTable T:zipLength' rule.iface; then
    echo 'The changed final Core equation escaped.' >&2
    exit 1
fi

# This source explicitly has the new result. A warm client must be rechecked.
cd "$out/provider"
sed 's/S (zipLength xs ys)/S Z/; s/(_ : xs) (_ : ys)/(_ : _) (_ : _)/' \
    "$src/ZipLengthSource.hs" >ZipLengthSource.hs
"${ghc[@]}" -O2 "${strip[@]}" -fno-full-laziness -c ZipLengthSource.hs
"${ghc[@]}" --show-iface ZipLengthSource.hi >changed.iface
grep -Fq 'FinalCoreSourceTable T:zipLength' changed.iface
cp ZipLengthSource.hi ../reader/
cd "$out/reader"
reject '^Laws.hs:16:12-15: error: \[GHC-83865\]' -fenable-rewrite-rules Laws.hs

cd "$out/nocode"
cp ../provider/PackedVecLibrary.hi "$src/ZipLengthSource.hs" .
"${ghc[@]}" "${strip[@]}" -fno-code -fwrite-interface -c ZipLengthSource.hs
"${ghc[@]}" --show-iface ZipLengthSource.hi >nocode.iface
if grep -Fq 'FinalCoreSourceTable T:zipLength' nocode.iface; then
    echo 'A no-code producer exported a provisional table.' >&2
    exit 1
fi

cd "$out/counting"
cp ../provider/PackedVecLibrary.hi "$src/ZipLengthSource.hs" .
"${ghc[@]}" "${strip[@]}" -fhpc -hpcdir . -c ZipLengthSource.hs
"${ghc[@]}" --show-iface ZipLengthSource.hi >counting.iface
if grep -Fq 'FinalCoreSourceTable T:zipLength' counting.iface; then
    echo 'Coverage ticks qualified as debugger breakpoints.' >&2
    exit 1
fi
