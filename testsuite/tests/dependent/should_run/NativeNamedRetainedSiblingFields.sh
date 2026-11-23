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
    cp "$src/"{PackedVecLibrary,NamedSiblingColumns}.hs .
    "${ghc[@]}" "$opt" --make -no-link NamedSiblingColumns.hs
    cp ./*.hi ../client/

    cd ../client
    test ! -e NamedSiblingColumns.hs
    cat > Main.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, OverloadedRecordDot #-}
import NamedSiblingColumns
import PackedVecLibrary (Nat(..), Vec(..), natToInt, toList, zipVec)

readPairs :: NamedColumns -> [(Char, Bool)]
readPairs p = toList (zipVec (namedLetters p) (namedFlags p))

readOnlyFlags :: NamedColumns -> [Bool]
readOnlyFlags NamedColumns{namedFlags = xs} = toList xs

main :: IO ()
main = do
    let p = NamedColumns { namedFlags = VCons True VNil
                         , storedLength = S Z
                         , namedLetters = VCons 'r' VNil }
    print (readPairs p)
    print (natToInt ((NamedColumns Z undefined undefined).storedLength))
    print (readOnlyFlags (NamedColumns { namedFlags = VCons True VNil
                                       , storedLength = S Z
                                       , namedLetters = undefined }))
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o ../provider/{PackedVecLibrary,NamedSiblingColumns}.o -o observe
    timeout 15 ./observe > actual
    printf '%s\n' "[('r',True)]" "0" '[True]' | cmp - actual

    cat > ProjectionType.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module ProjectionType where
import NamedSiblingColumns
import PackedVecLibrary (Vec)

checked :: foreach (p :: NamedColumns) -> Vec Char (storedLength p)
checked p = namedLetters p
HS
    "${ghc[@]}" "$opt" -c ProjectionType.hs

    cat > Existing.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module Existing where
import PackedVecLibrary (Nat(..), Vec(..))

data GadtColumns where
    GadtColumns :: { gLength :: Nat
                   , gLetters :: Vec Char gLength
                   , gFlags :: Vec Bool gLength
                   } -> GadtColumns

data Scoped (sLength :: Nat) = Scoped
    { sLength :: Nat, sLetters :: Vec Char sLength, sFlags :: Vec Bool sLength }

unrelatedGadtLength :: GadtColumns
unrelatedGadtLength = GadtColumns (S Z) VNil VNil

unrelatedScopedLength :: Scoped 'Z
unrelatedScopedLength = Scoped (S Z) VNil VNil
HS
    "${ghc[@]}" "$opt" -c Existing.hs

    reject() {
        local pattern=$1
        shift
        if "${ghc[@]}" "$opt" -c "$@" > rejected.log 2>&1 ||
           ! grep -Eq "$pattern" rejected.log; then
            cat rejected.log >&2
            exit 1
        fi
    }

    cat > MissingPayloads.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (RecConError(..), evaluate, try)
import Data.List (isInfixOf)
import NamedSiblingColumns
import PackedVecLibrary (Nat(..), Vec(..), natToInt, toList)

letters :: NamedColumns -> [Char]
letters p = toList (namedLetters p)

flags :: NamedColumns -> [Bool]
flags p = toList (namedFlags p)

missingOne :: NamedColumns
missingOne = NamedColumns { storedLength = S Z, namedFlags = VCons True VNil }

missingBoth :: NamedColumns
missingBoth = NamedColumns { storedLength = S Z }

checkMissing :: String -> a -> IO ()
checkMissing field value = do
    result <- try (evaluate value >> pure ())
    case result of
        Left (RecConError msg) | field `isInfixOf` msg -> putStrLn field
        _ -> fail ("expected the missing-field exception for " ++ field)

main :: IO ()
main = do
    print (flags missingOne)
    checkMissing "namedLetters" (letters missingOne)
    print (natToInt (storedLength missingBoth))
    checkMissing "namedLetters" (letters missingBoth)
    checkMissing "namedFlags" (flags missingBoth)
HS
    "${ghc[@]}" "$opt" -Wmissing-fields -c MissingPayloads.hs > missing.log 2>&1 ||
        { cat missing.log >&2; exit 1; }
    test "$(grep -Fc -- '[-Wmissing-fields]' missing.log)" = 2
    "${ghc[@]}" MissingPayloads.o ../provider/{PackedVecLibrary,NamedSiblingColumns}.o -o missing
    timeout 15 ./missing > actual
    printf '%s\n' '[True]' 'namedLetters' '1' 'namedLetters' 'namedFlags' | cmp - actual
    reject 'GHC-20125.*-Werror=missing-fields' -fforce-recomp -Werror=missing-fields MissingPayloads.hs

    cat > MissingAtVariable.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module MissingAtVariable where
import NamedSiblingColumns
import PackedVecLibrary (Nat, Vec)

onlyFlags :: foreach (n :: Nat) -> Vec Bool n -> NamedColumns
onlyFlags n flags = NamedColumns { storedLength = n, namedFlags = flags }
HS
    "${ghc[@]}" "$opt" -Wmissing-fields -c MissingAtVariable.hs > missing-variable.log 2>&1 ||
        { cat missing-variable.log >&2; exit 1; }
    grep -Fq -- '[-Wmissing-fields]' missing-variable.log

    cat > MissingWitness.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module MissingWitness where
import NamedSiblingColumns
import PackedVecLibrary (Vec(..))

wrong :: NamedColumns
wrong = NamedColumns { namedLetters = VNil, namedFlags = VNil }
HS
    reject 'GHC-83605' MissingWitness.hs
    grep -Fq 'MissingWitness.hs:7:9:' rejected.log
    grep -Eq 'NamedColumns.*requires witness field.*storedLength' rejected.log

    cat > ForeignField.hs <<'HS'
{-# LANGUAGE NoDisambiguateRecordFields, NoExperimentalDependentTypes #-}
module ForeignField where
import NamedSiblingColumns
import PackedVecLibrary (Nat(..))

data Other = Other { foreignField :: Bool }

wrong :: NamedColumns
wrong = NamedColumns { storedLength = Z, foreignField = True }
HS
    reject 'GHC-53822' ForeignField.hs

    cat > BadConstruction.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module BadConstruction where
import NamedSiblingColumns
import PackedVecLibrary (Nat(..), Vec(..))

wrong :: NamedColumns
wrong = NamedColumns { storedLength = S Z, namedLetters = VNil, namedFlags = VNil }
HS
    reject 'GHC-(05617|83865)' BadConstruction.hs

    cat > BadSelection.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, OverloadedRecordDot #-}
module BadSelection where
import NamedSiblingColumns
import PackedVecLibrary (Nat(..), Vec)

wrong :: NamedColumns -> Vec Bool 'Z
wrong p = p.namedFlags
HS
    reject 'GHC-39999' BadSelection.hs

    cat > BadUpdate.hs <<'HS'
module BadUpdate where
import NamedSiblingColumns
import PackedVecLibrary (Nat(..))

wrong :: NamedColumns -> NamedColumns
wrong p = p { storedLength = Z }
HS
    reject 'Record update.*retained' BadUpdate.hs

    cat > BadZip.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module BadZip where
import NamedSiblingColumns
import PackedVecLibrary (toList, zipVec)

wrong :: NamedColumns -> NamedColumns -> [(Char, Bool)]
wrong p q = toList (zipVec (namedLetters p) (namedFlags q))
HS
    reject 'GHC-(05617|83865)' BadZip.hs

    cat > BadFieldType.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module BadFieldType where
import PackedVecLibrary (Nat, Vec)

data BadFieldType = BadFieldType
    { badLength :: Nat, badPayload :: (Vec Char badLength, Bool) }
HS
    reject 'Each payload field of a retained record' BadFieldType.hs

    cat > BadRecordSyntax.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module BadRecordSyntax where
import PackedVecLibrary (Nat, Vec)

data Bad where
    Bad :: foreach (n :: Nat) -> { badField :: Vec Char n } -> Bad
HS
    reject 'GHC-89246' BadRecordSyntax.hs
done
