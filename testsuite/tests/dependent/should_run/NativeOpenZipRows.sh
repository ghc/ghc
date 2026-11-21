#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -O0 -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans -Werror=inaccessible-code -i -i.)
strip=(-fomit-interface-pragmas -fno-write-if-simplified-core)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,reader,rule,nocode,nocode-reader}

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
"${ghc[@]}" "${strip[@]}" -O2 -c ZipLengthSource.hs
"${ghc[@]}" --show-iface ZipLengthSource.hi >provider.iface
grep -Fq 'FinalCoreSourceTable T:zipLength' provider.iface

cat >ZipVariants.hs <<'HS'
module ZipVariants (zipConcrete, loopLength) where
import PackedVecLibrary (Nat(..))

zipConcrete :: [Bool] -> [Char] -> Nat
zipConcrete [] _ = Z
zipConcrete (_ : _) [] = Z
zipConcrete (_ : xs) (_ : ys) = S (zipConcrete xs ys)

loopLength :: [Bool] -> [Char] -> Nat
loopLength [] _ = Z
loopLength (_ : _) [] = Z
loopLength left@(_ : _) (_ : ys) = S (loopLength left ys)
HS
"${ghc[@]}" "${strip[@]}" -c ZipVariants.hs
"${ghc[@]}" --show-iface ZipVariants.hi >variants.iface
grep -Fq 'FinalCoreSourceTable T:zipConcrete' variants.iface
if grep -Fq 'FinalCoreSourceTable T:loopLength' variants.iface; then
    echo 'The nondecreasing call exported a table.' >&2
    exit 1
fi

cd "$out/reader"
cp ../provider/{PackedVecLibrary,ZipLengthSource,ZipVariants}.hi "$src/ZipRows.hs" .
cat >Laws.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, GADTs #-}
module Laws where
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..))
import ZipLengthSource (zipLength)
import ZipVariants (zipConcrete)

emptyLeft :: forall (ys :: [Bool]). zipLength ('[] :: [Char]) ys :~: 'Z
emptyLeft = Refl

emptyRight :: forall (x :: Char) (xs :: [Char]). zipLength (x ': xs) ('[] :: [Bool]) :~: 'Z
emptyRight = Refl

bothCons :: forall (x :: Char) (xs :: [Char]) (y :: Bool) (ys :: [Bool]).
            zipLength (x ': xs) (y ': ys) :~: 'S (zipLength xs ys)
bothCons = Refl

concrete :: forall (ys :: [Char]). zipConcrete '[] ys :~: 'Z
concrete = Refl
HS
"${ghc[@]}" -fenable-rewrite-rules -c Laws.hs
"${ghc[@]}" -c ZipRows.hs

cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeApplications #-}
module Main where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import PackedVecLibrary (toList)
import ZipRows (zipRows)

main :: IO ()
main = do
    print (toList (zipRows "ab" [True]), toList (zipRows "a" [True, False]))
    let infinite = repeat 'a'
        badFirst = error "first spine" :: String
        badSecond = error "second spine" :: [Bool]
        badChar = error "first element" :: Char
        badBool = error "second element" :: Bool
        emptyB = [] :: [Bool]
    print (toList (zipRows infinite [True, False]))
    print (null (toList (zipRows "" badSecond)))
    print (length (toList (zipRows [badChar] [badBool])))
    first <- try @ErrorCall (evaluate (null (toList (zipRows badFirst emptyB))))
    print (either (isPrefixOf "first spine" . displayException) (const False) first)
HS
"${ghc[@]}" -c Main.hs
"${ghc[@]}" Main.o ZipRows.o ../provider/{PackedVecLibrary,ZipLengthSource}.o -o run
./run >actual
diff -u - actual <<'OUT'
([('a',True)],[('a',True)])
[('a',True),('a',False)]
True
1
True
OUT

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

{ sed -n '1,4p' WrongDemand.hs | sed 's/WrongDemand/Nondecreasing/'
  cat <<'HS'
import ZipVariants (loopLength)
wrong :: forall (ys :: [Char]). loopLength '[] ys :~: 'Z
wrong = Refl
HS
} >Nondecreasing.hs
reject '^Nondecreasing.hs:6:33-42: error: \[GHC-70565\]' Nondecreasing.hs

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
"${ghc[@]}" "${strip[@]}" -O1 -fno-full-laziness -ddump-rule-firings \
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
"${ghc[@]}" "${strip[@]}" -O2 -fno-full-laziness -c ZipLengthSource.hs
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
