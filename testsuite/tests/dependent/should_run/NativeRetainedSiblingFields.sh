#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -fomit-interface-pragmas -fno-write-if-simplified-core)

for opt in -O0 -O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/"{PackedVecLibrary,SiblingColumns,TypeLambda}.hs .
    "${ghc[@]}" "$opt" --make -no-link SiblingColumns.hs TypeLambda.hs
    "${ghc[@]}" --show-iface SiblingColumns.hi > columns.iface
    grep -Fq 'SourceTable T:size' columns.iface
    grep -Fq 'retained-sibling-fields-v1' columns.iface
    cp ./*.hi "$src/"{SiblingColumnsClient,SiblingColumnRow}.hs ../client/

    cd ../client
    test ! -e SiblingColumns.hs
    test ! -e PackedVecLibrary.hs
    "${ghc[@]}" "$opt" -c SiblingColumnsClient.hs
    "${ghc[@]}" "$opt" -c SiblingColumnRow.hs
    mv SiblingColumnRow.hs SiblingColumnRow.hidden
    cat > KnownLength.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module KnownLength where
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), Vec(..))
import SiblingColumns (Columns(..), size)

#ifdef WRONG
known :: size ('Columns 'Z 'VNil 'VNil) :~: 'S 'Z
#else
known :: size ('Columns 'Z 'VNil 'VNil) :~: 'Z
#endif
known = Refl
HS
    "${ghc[@]}" "$opt" -c KnownLength.hs
    cat > Main.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import PackedVecLibrary (Nat(..), Vec(..), natToInt, toList)
import SiblingColumnRow (packRow)
import SiblingColumns (Columns(..), bottomFields, flags, letters, size)
import SiblingColumnsClient (readPairs)
import TypeLambda (Fields(..))

readLetters :: Columns -> String
readLetters p = toList (letters p)

readFlags :: Columns -> [Bool]
readFlags p = toList (flags p)

throws :: String -> a -> IO Bool
throws label value = do
    result <- try (evaluate (value `seq` ())) :: IO (Either ErrorCall ())
    pure (either (isPrefixOf label . displayException) (const False) result)

main :: IO ()
main = do
    ls <- throws "forced letters" (readLetters bottomFields)
    bs <- throws "forced flags" (readFlags bottomFields)
    print (natToInt (size (Columns (S Z) undefined undefined)),
           readLetters (Columns (S Z) (VCons 'r' VNil) undefined),
           readFlags (Columns (S Z) undefined (VCons True VNil)),
           ls, bs)
    let columns = Columns (S (S Z))
            (VCons 'r' (VCons 's' VNil))
            (VCons True (VCons False VNil))
    print (readPairs columns)
    case packRow columns of
        Field xs (Field ys End) -> print (toList xs, toList ys)
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o SiblingColumnsClient.o SiblingColumnRow.o \
        ../provider/{PackedVecLibrary,SiblingColumns,TypeLambda}.o -o observe
    timeout 15 ./observe > actual
    printf '%s\n' '(1,"r",[True],True,True)' "[('r',True),('s',False)]" \
        '("rs",[True,False])' | cmp - actual

    reject() {
        local code=$1
        shift
        if "${ghc[@]}" "$opt" -fforce-recomp -c "$@" > rejected.log 2>&1 ||
           ! grep -Eq "$code" rejected.log; then
            cat rejected.log >&2
            exit 1
        fi
    }
    cat > WrongPairs.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module WrongPairs where
import PackedVecLibrary (toList, zipVec)
import SiblingColumns (Columns, flags, letters)

readPairs :: Columns -> Columns -> [(Char, Bool)]
readPairs p q = toList (zipVec (letters p) (flags q))
HS
    reject 'GHC-(05617|83865)' WrongPairs.hs
    grep -Fq 'size p' rejected.log
    grep -Fq 'size q' rejected.log
    grep -Eiq "second argument of .*zipVec.*flags q" rejected.log

    cat > WrongRow.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module WrongRow where
import PackedVecLibrary (Vec)
import SiblingColumns (Columns, flags, letters, size)
import TypeLambda (Fields(..), Map)

wrong :: foreach (p :: Columns) -> foreach (q :: Columns) ->
         Fields (Map (\a -> Vec a (size p)) '[Char, Bool])
wrong p q = Field (letters p) (Field (flags q) End)
HS
    reject 'GHC-(05617|83865)' WrongRow.hs
    grep -Fq 'flags q' rejected.log

    reject 'GHC-(05617|83865)' -DWRONG KnownLength.hs
    cat > WrongFlags.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module WrongFlags where
import PackedVecLibrary (Nat(..), Vec)
import SiblingColumns (Columns(..), size)

wrong :: foreach (p :: Columns) -> Vec Bool ('S (size p))
wrong (Columns _ _ bs) = bs
HS
    reject 'GHC-(05617|83865)' WrongFlags.hs
    cat > StrictSize.hs <<'HS'
{-# LANGUAGE BangPatterns, GADTs, NoExperimentalDependentTypes #-}
module StrictSize where
import PackedVecLibrary (Nat)
import SiblingColumns (Columns(..))

forceLetters :: Columns -> Nat
forceLetters (Columns n !_ _) = n

forceFlags :: Columns -> Nat
forceFlags (Columns n _ !_) = n
HS
    "${ghc[@]}" "$opt" -fwrite-source-equations -c StrictSize.hs
    "${ghc[@]}" --show-iface StrictSize.hi > strict.iface
    if grep -Eq 'SourceTable T:force(Letters|Flags)' strict.iface; then
        cat strict.iface >&2
        exit 1
    fi
done
