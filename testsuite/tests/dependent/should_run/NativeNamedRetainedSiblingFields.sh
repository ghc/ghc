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
import Control.Exception (ErrorCall, evaluate, try)
import Data.Char (toUpper)
import NamedSiblingColumns
import qualified NamedSiblingColumns as Named
import PackedVecLibrary (Nat(..), Vec(..), natToInt, previous, replicateNative, toList, zipVec)

readPairs :: NamedColumns -> [(Char, Bool)]
readPairs p = toList (zipVec (namedLetters p) (namedFlags p))

fillFromWitness :: NamedColumns -> [Char]
fillFromWitness p = toList (replicateNative (Named.storedLength p) '-')

fillFromPrevious :: NamedColumns -> [Char]
fillFromPrevious p = toList (replicateNative (previous (storedLength p)) '-')

readOnlyFlags :: NamedColumns -> [Bool]
readOnlyFlags NamedColumns{namedFlags = xs} = toList xs

mapVector :: (a -> b) -> Vec a n -> Vec b n
mapVector _ VNil = VNil
mapVector f (VCons x xs) = VCons (f x) (mapVector f xs)

negateFlags :: NamedColumns -> NamedColumns
negateFlags p = p { namedFlags = mapVector not (namedFlags p) }

updateBoth :: NamedColumns -> NamedColumns
updateBoth p = p { namedFlags = mapVector not (namedFlags p)
                 , namedLetters = mapVector toUpper (namedLetters p) }

updateOnlyDemanded :: NamedColumns -> NamedColumns
updateOnlyDemanded p = p { namedLetters = undefined
                         , namedFlags = mapVector not (namedFlags p) }

writeSameWitness :: NamedColumns -> NamedColumns
writeSameWitness p = p { Named.storedLength = Named.storedLength p
                       , namedFlags = mapVector not (namedFlags p) }

writeOnlyWitness :: NamedColumns -> NamedColumns
writeOnlyWitness p = p { storedLength = (storedLength (p)) }

omitFlags :: NamedColumns -> NamedColumns
omitFlags p = p { namedFlags = undefined }

emptyRecord :: NamedColumns -> NamedColumns
emptyRecord p = p { namedFlags = VNil, storedLength = Z, namedLetters = VNil }

main :: IO ()
main = do
    let p = NamedColumns { namedFlags = VCons True VNil
                         , storedLength = S Z
                         , namedLetters = VCons 'r' VNil }
    print (readPairs p)
    let unusedPayloads = NamedColumns (S (S Z)) undefined undefined
    print (fillFromWitness unusedPayloads, fillFromPrevious unusedPayloads)
    print (natToInt ((NamedColumns Z undefined undefined).storedLength))
    print (readOnlyFlags (NamedColumns { namedFlags = VCons True VNil
                                       , storedLength = S Z
                                       , namedLetters = undefined }))
    let q = negateFlags p
    print (natToInt (storedLength q), readPairs q)
    print (readOnlyFlags (negateFlags (NamedColumns (S Z) undefined (VCons True VNil))))
    let r = omitFlags p
    print (toList (namedLetters r))
    let both = updateBoth p
    print (natToInt (storedLength both), readPairs both)
    print (readOnlyFlags (updateOnlyDemanded p))
    let same = writeSameWitness p
    print (natToInt (storedLength same), readPairs same)
    print (readPairs (writeOnlyWitness p))
    let lazyLength :: Nat
        lazyLength = undefined
    print (writeOnlyWitness (NamedColumns lazyLength undefined undefined) `seq` True)
    let empty = emptyRecord p
    print (natToInt (storedLength empty), readPairs empty)
    let noOldPayloads = emptyRecord (NamedColumns (S Z) undefined undefined)
    print (readPairs noOldPayloads)
    oldBottom <- try (evaluate (emptyRecord undefined)) :: IO (Either ErrorCall NamedColumns)
    print (either (const True) (const False) oldBottom)
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o ../provider/{PackedVecLibrary,NamedSiblingColumns}.o -o observe
    timeout 15 ./observe > actual
    printf '%s\n' "[('r',True)]" '("--","-")' "0" '[True]' "(1,[('r',False)])" '[False]' '"r"' \
        "(1,[('R',False)])" '[False]' "(1,[('r',False)])" "[('r',True)]" 'True' \
        '(0,[])' '[]' 'True' | cmp - actual

    cat > ProjectionType.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, TypeOperators #-}
module ProjectionType where
import Data.Type.Equality ((:~:)(Refl))
import NamedSiblingColumns
import PackedVecLibrary (Nat, Vec, replicateNative)

checked :: foreach (p :: NamedColumns) -> Vec Char (storedLength p)
checked p = namedLetters p

fillAtWitness :: foreach (p :: NamedColumns) -> Vec Char (storedLength p)
fillAtWitness p = replicateNative (storedLength p) '-'

replaceFlags :: foreach (p :: NamedColumns) -> Vec Bool (storedLength p) -> NamedColumns
replaceFlags p flags = p { namedFlags = flags }

replaceAll :: NamedColumns -> foreach (n :: Nat) -> Vec Char n -> Vec Bool n -> NamedColumns
replaceAll p n letters flags = p { storedLength = n, namedLetters = letters, namedFlags = flags }

updateWithProof :: foreach (p :: NamedColumns) (q :: NamedColumns)
                -> (storedLength p :~: storedLength q) -> NamedColumns
updateWithProof p q Refl = p { storedLength = storedLength q, namedFlags = namedFlags q }
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

    cat > OrdinarySelectorOperand.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module OrdinarySelectorOperand where
import PackedVecLibrary (Nat, replicateNative, toList)

data Plain = Plain { counted :: Nat }

fill :: Plain -> [Char]
fill p = toList (replicateNative (counted p) '-')
HS
    reject 'GHC-83605' OrdinarySelectorOperand.hs
    grep -Fq 'GHC cannot use this expression as a retained index' rejected.log

    cat > ShadowSelectorOperand.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module ShadowSelectorOperand where
import NamedSiblingColumns (NamedColumns)
import PackedVecLibrary (Nat(..), replicateNative, toList)

fill :: NamedColumns -> [Char]
fill p = let storedLength _ = Z in toList (replicateNative (storedLength p) '-')
HS
    reject 'GHC-83605' ShadowSelectorOperand.hs
    grep -Fq 'No checked source equations' rejected.log

    cat > PreviousRule.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module PreviousRule where
import PackedVecLibrary (Nat(..), previous)
{-# RULES "previous/reset" forall n. previous n = Z #-}
HS
    "${ghc[@]}" "$opt" -fenable-rewrite-rules -fno-omit-interface-pragmas -c PreviousRule.hs
    cat > RuleSelectorOperand.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module RuleSelectorOperand where
import NamedSiblingColumns
import PackedVecLibrary (previous, replicateNative, toList)
import PreviousRule ()

fill :: NamedColumns -> [Char]
fill p = toList (replicateNative (previous (storedLength p)) '-')
HS
    reject 'GHC-83605' -fenable-rewrite-rules -fno-ignore-interface-pragmas RuleSelectorOperand.hs
    grep -Eq 'A rewrite rule can change the result of.*previous' rejected.log

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
    reject 'GHC-20125.*Werror=missing-fields' -fforce-recomp -Werror=missing-fields MissingPayloads.hs

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

    cat > OrdinaryStrict.hs <<'HS'
module OrdinaryStrict where

data Ordinary = Ordinary { strictField :: !Bool, lazyField :: Int }

value :: Ordinary
value = Ordinary { lazyField = 0 }
HS
    reject 'GHC-95909' OrdinaryStrict.hs

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
    reject 'GHC-83605' BadUpdate.hs
    grep -Fq 'BadUpdate.hs:6:15:' rejected.log
    grep -Eq 'requires replacement for fields .*namedLetters.*namedFlags' rejected.log

    cat > MissingChangedPayload.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module MissingChangedPayload where
import NamedSiblingColumns
import PackedVecLibrary (Nat(..), Vec(..))

wrong :: NamedColumns
wrong =
  let old = NamedColumns (S Z) (VCons 'x' VNil) (VCons True VNil)
  in old { storedLength = Z, namedFlags = VNil }
HS
    reject 'GHC-83605' MissingChangedPayload.hs
    grep -Fq 'MissingChangedPayload.hs:9:12:' rejected.log
    grep -A1 -E 'Updating witness field .*storedLength.*of constructor' rejected.log |
        grep -Fq 'NamedColumns'
    grep -Eq 'requires replacement for field .*namedLetters' rejected.log

    cat > OtherWitness.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module OtherWitness where
import NamedSiblingColumns

wrong :: NamedColumns -> NamedColumns -> NamedColumns
wrong p q =
  p { storedLength = storedLength q, namedFlags = namedFlags q }
HS
    reject 'GHC-83605' OtherWitness.hs
    grep -Fq 'OtherWitness.hs:7:7:' rejected.log
    grep -Eq 'requires replacement for field .*namedLetters' rejected.log

    cat > ShadowWitness.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module ShadowWitness where
import NamedSiblingColumns
import PackedVecLibrary (Nat(..), Vec(..))

wrong :: NamedColumns -> NamedColumns
wrong p =
  let storedLength _ = Z
  in p { storedLength = storedLength p, namedFlags = VNil }
HS
    reject 'GHC-83605' ShadowWitness.hs
    grep -Eq 'requires replacement for field .*namedLetters' rejected.log

    cat > WrongNewPayload.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module WrongNewPayload where
import NamedSiblingColumns
import PackedVecLibrary (Nat(..), Vec(..))

wrong :: NamedColumns -> NamedColumns
wrong p = p { storedLength = Z
            , namedLetters = VCons 'x' VNil
            , namedFlags = VNil }
HS
    reject 'GHC-(05617|83865)' WrongNewPayload.hs
    grep -Fq 'WrongNewPayload.hs:8:30:' rejected.log

    cat > StrictUpdate.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, Strict #-}
module Main where
import Control.Exception (ErrorCall, evaluate, try)
import NamedSiblingColumns
import PackedVecLibrary (Nat(..), Vec(..))

clear :: NamedColumns -> NamedColumns
clear p = p { storedLength = Z, namedLetters = undefined, namedFlags = VNil }

writeWitness :: NamedColumns -> NamedColumns
writeWitness p = p { storedLength = storedLength p }

data Ordinary = Ordinary { field :: ~Bool }
update :: Ordinary -> Ordinary
update p = p { field = undefined }

checkDemand :: a -> IO ()
checkDemand ~value = do
    forced <- try (evaluate value >> pure ()) :: IO (Either ErrorCall ())
    print (either (const True) (const False) forced)

main :: IO ()
main = do
    checkDemand (clear (NamedColumns Z VNil VNil))
    checkDemand (update (Ordinary False))
    checkDemand (writeWitness (NamedColumns Z undefined undefined))
    let missingLength :: Nat
        ~missingLength = undefined
    checkDemand (writeWitness (NamedColumns missingLength undefined undefined))
HS
    "${ghc[@]}" "$opt" -c StrictUpdate.hs
    "${ghc[@]}" StrictUpdate.o ../provider/{PackedVecLibrary,NamedSiblingColumns}.o -o strict-update
    timeout 15 ./strict-update > actual
    printf '%s\n' 'True' 'True' 'False' 'True' | cmp - actual

    cat > BadPayloadUpdate.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module BadPayloadUpdate where
import NamedSiblingColumns

-- Both records can store the same length. Their projections are independent.
wrong :: NamedColumns -> NamedColumns -> NamedColumns
wrong p q = p { namedFlags = namedFlags q }
HS
    reject 'GHC-(05617|83865)' BadPayloadUpdate.hs

    cat > BadSiblingUpdate.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module BadSiblingUpdate where
import NamedSiblingColumns

wrong :: NamedColumns -> NamedColumns -> NamedColumns
wrong p q = p { namedLetters = namedLetters p
              , namedFlags = namedFlags q }
HS
    reject 'GHC-(05617|83865)' BadSiblingUpdate.hs
    grep -Fq 'BadSiblingUpdate.hs:7:30:' rejected.log

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
