#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -fdiagnostics-color=never -dcore-lint -dstg-lint)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
producer_flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
if [[ " $* " != *" -dynamic "* ]] &&
   "${ghc[@]}" --info | grep -F '("Support dynamic-too","YES")' >/dev/null; then
    producer_flags+=(-dynamic-too)
fi

for opt in -O0 -O2; do
    mkdir -p "$out/${opt#-}"/{producer,client}
    cd "$out/${opt#-}/producer"
    cp "$src"/{ExistentialVec,IndexedVec,WitnessedBatch}.hs .
    cat >WrongWitness.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module WrongWitness
    ( buildWrong, wrongMask, wrongColumns, BadField(..), badField
    , wrongRows, wrongCols, Nested(..), nested
    , EvidenceBatch(..), withEvidence, TaggedBatch(..), withEquality
    ) where
import Data.Proxy (Proxy)
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)
import WitnessedBatch (Batch(..), MaskedRow(..), KeyedRow(..), TableMargins(..), tableMargins)

buildWrong :: Natural -> Batch Natural
buildWrong n = Chunk (SS SZ) (VCons n VNil)
                     (Chunk (SS SZ) (unsafeCoerce (VCons n (VCons n VNil))) End)

wrongMask :: Natural -> MaskedRow Natural
wrongMask n = MaskedRow (SS SZ) (VCons n VNil) 'x' (unsafeCoerce (VNil :: Vec 'Z Bool))

wrongColumns :: Natural -> KeyedRow Natural
wrongColumns n = KeyedRow 'r' (VCons 'x' VNil) (unsafeCoerce (VCons n (VCons n VNil)))

wrongRows, wrongCols :: Natural -> TableMargins Natural
wrongRows n = case tableMargins n of
    TableMargins r c _ ys -> TableMargins r c (unsafeCoerce ys) ys
wrongCols n = case tableMargins n of
    TableMargins r c xs _ -> TableMargins r c xs (unsafeCoerce xs)

data Nested a where
    Nested :: Vec cols Char -> Vec rows (Vec cols a) -> Nested a

nested :: Natural -> Nested Natural
nested _ = error "unsupported nested field was evaluated"

data BadField a where
    BadField :: SN n -> Vec n a -> Char -> Proxy n -> BadField a

badField :: Natural -> BadField Natural
badField _ = error "unsupported field was evaluated"

data EvidenceBatch a where
    EvidenceEnd :: EvidenceBatch a
    EvidenceChunk :: Eq a => SN n -> Vec n a -> EvidenceBatch a -> EvidenceBatch a

withEvidence :: Natural -> EvidenceBatch Natural
withEvidence _ = error "class-constrained constructor was evaluated"

data TaggedBatch (marked :: Bool) a where
    TaggedChunk :: SN n -> Vec n a -> TaggedBatch 'True a

withEquality :: Natural -> TaggedBatch 'True Natural
withEquality _ = error "equality-constrained constructor was evaluated"
HS
    "${ghc[@]}" "$opt" "${producer_flags[@]}" -c IndexedVec.hs ExistentialVec.hs WitnessedBatch.hs WrongWitness.hs
    cd "../client"
    cp ../producer/*.{hi,o} .
    test ! -e WitnessedBatch.hs
    if [[ -f ../producer/WitnessedBatch.dyn_o ]]; then cp ../producer/*.{dyn_hi,dyn_o} .; fi
    cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Proof (evenBatch, oddBatch, mask, empty, row, margins) where

import Data.Type.Equality ((:~:)(Refl))
import qualified ExistentialVec as E
import qualified IndexedVec as V
import qualified WitnessedBatch as B

evenBatch :: B.build 4 :~: 'B.Chunk ('E.SS 'E.SZ) ('V.VCons 4 'V.VNil)
                            ('B.Chunk ('E.SS ('E.SS 'E.SZ))
                                ('V.VCons 5 ('V.VCons 6 'V.VNil)) 'B.End)
evenBatch = Refl

oddBatch :: B.build 3 :~: 'B.Chunk ('E.SS ('E.SS 'E.SZ)) ('V.VCons 3 ('V.VCons 4 'V.VNil)) 'B.End
oddBatch = Refl

mask :: B.maskedRow 4 :~: 'B.MaskedRow ('E.SS ('E.SS 'E.SZ))
                            ('V.VCons 4 ('V.VCons 5 'V.VNil))
                            'x' ('V.VCons 'False ('V.VCons 'True 'V.VNil))
mask = Refl

empty :: B.keyedRow 0 :~: 'B.KeyedRow 'e' 'V.VNil 'V.VNil
empty = Refl

row :: B.keyedRow 4 :~: 'B.KeyedRow 'r' ('V.VCons 'x' ('V.VCons 'y' 'V.VNil))
                                        ('V.VCons 4 ('V.VCons 5 'V.VNil))
row = Refl

margins :: B.tableMargins 0 :~: 'B.TableMargins 'V.VNil ('V.VCons 'x' 'V.VNil)
                                                'V.VNil ('V.VCons 0 'V.VNil)
margins = Refl
HS
    cat >Demand.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Demand (firstWidth, rowWidth) where
import Data.Type.Equality ((:~:)(Refl))
import qualified WitnessedBatch as B (firstWidth, partial, rowWidth, partialRow)

firstWidth :: B.firstWidth (B.partial 9) :~: 1
firstWidth = Refl

rowWidth :: B.rowWidth (B.partialRow 9) :~: 1
rowWidth = Refl
HS
    cat >Main.hs <<'HS'
module Main where
import qualified Demand
import qualified ExistentialVec as E
import qualified IndexedVec as V
import Proof (evenBatch, oddBatch, mask, row, empty, margins)
import qualified WitnessedBatch as B

skipped :: [Int]
skipped = B.selectedValues (B.MaskedRow (E.SS E.SZ) (V.VCons (error "unused value") V.VNil)
                            (error "unused label") (V.VCons False V.VNil))

main :: IO ()
main = do
    print (evenBatch, oddBatch, mask, Demand.firstWidth, Demand.rowWidth,
           B.firstWidth (B.build 4), B.firstWidth (B.build 3), B.firstWidth (B.partial 9),
           B.selectedValues (B.maskedRow 4), skipped)
    let short :: B.KeyedRow Int
        short = B.KeyedRow (error "unused tag") (V.VCons 'x' V.VNil)
                           (V.VCons 8 (error "unused tail"))
    print (row, empty, B.lookupColumn 'y' (B.keyedRow 4), B.lookupColumn 'x' short)
    let partial :: B.TableMargins Int
        partial = B.TableMargins (error "unused row labels") (V.VCons 'x' V.VNil)
                                 (error "unused row totals") (V.VCons 7 V.VNil)
    print (margins, B.lookupTotals 'r' 'y' (B.tableMargins 4), snd (B.lookupTotals 'r' 'x' partial))
HS
    "${ghc[@]}" "$opt" -c Proof.hs Demand.hs
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" ./*.o -o run
    test "$(./run)" = $'(Refl,Refl,Refl,Refl,Refl,1,2,1,[5],[])\n(Refl,Refl,Just 5,Just 8)\n(Refl,(Just 8,Just 4),Just 7)'

    cat >WrongLength.hs <<'HS'
module WrongLength where
import ExistentialVec (SN(..))
import IndexedVec (Vec(..))
import WitnessedBatch (MaskedRow(..))
bad :: MaskedRow Int
bad = MaskedRow (SS SZ) (VCons 4 VNil) 'x' VNil
HS
    if "${ghc[@]}" "$opt" -c WrongLength.hs >length.log 2>&1 ||
       ! grep -Eq 'WrongLength.hs:6:[0-9]+: error: \[GHC-83865\]' length.log ||
       ! grep -Fq 'fourth argument' length.log; then
        cat length.log >&2
        exit 1
    fi

    cat >SwapMargins.hs <<'HS'
module SwapMargins where
import WitnessedBatch (TableMargins(..))
swapped :: TableMargins a -> TableMargins a
swapped (TableMargins rowKeys colKeys rowTotals colTotals) =
    TableMargins rowKeys colKeys colTotals rowTotals
HS
    if "${ghc[@]}" "$opt" -c SwapMargins.hs >swap.log 2>&1 ||
       ! grep -Fq 'SwapMargins.hs:5:34: error: [GHC-25897]' swap.log ||
       ! grep -Fq 'third argument' swap.log; then
        cat swap.log >&2
        exit 1
    fi

    reject() {
        local module=$1 expected=$2
        if "${ghc[@]}" "$opt" -c "$module.hs" >reject.log 2>&1 ||
           ! grep -Eq "^${module}.hs:[0-9]+:[0-9]+: error: \\[GHC-70565\\]" reject.log ||
           ! grep -Fq "$expected" reject.log; then
            cat reject.log >&2
            exit 1
        fi
    }
    cat >DemandTail.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module DemandTail where
import Data.Proxy (Proxy(..))
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import qualified WitnessedBatch as B
full :: Proxy (B.partialTail 9)
full = Proxy
HS
    reject DemandTail 'unavailable next chunk'

    cat >Hidden.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module Hidden where
import Data.Proxy (Proxy(..))
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import qualified WitnessedBatch as B (Batch(Chunk), build)
hidden :: Proxy (B.build 4)
hidden = Proxy
HS
    reject Hidden 'bring every constructor into scope from a non-SOURCE import.'
    if grep -Fq "‘End’" reject.log; then cat reject.log >&2; exit 1; fi

    sed -e 's/module Hidden/module HiddenWitness/' -e 's/SN(..)/SN(SS)/' \
        -e 's/(Batch(Chunk), build)/(Batch(..), build)/' Hidden.hs >HiddenWitness.hs
    reject HiddenWitness 'Cannot inspect the constructors of SN.'
    if grep -Fq "‘SZ’" reject.log; then cat reject.log >&2; exit 1; fi

    cat >ClassEvidence.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module ClassEvidence where
import Data.Proxy (Proxy(..))
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import qualified WrongWitness as W
proof :: Proxy (W.withEvidence 4)
proof = Proxy
HS
    reject ClassEvidence 'EvidenceBatch Natural.'
    if grep -Fq 'class-constrained constructor was evaluated' reject.log; then
        cat reject.log >&2
        exit 1
    fi

    sed -e 's/module ClassEvidence/module EqualityEvidence/' \
        -e 's/withEvidence/withEquality/' ClassEvidence.hs >EqualityEvidence.hs
    reject EqualityEvidence 'Cannot recover a constructor-local type argument from the checked result type.'
    if grep -Fq 'equality-constrained constructor was evaluated' reject.log; then
        cat reject.log >&2
        exit 1
    fi

    sed -e 's/module DemandTail/module BadWitness/' -e 's/B.partialTail 9/Wrong.buildWrong 4/' \
        -e '/^import qualified WitnessedBatch as B$/a import qualified WrongWitness as Wrong' \
        DemandTail.hs >BadWitness.hs
    reject BadWitness 'The value returned by the interpreter does not match the checked result type.'

    sed -e 's/module BadWitness/module BadMask/' -e 's/Wrong.buildWrong/Wrong.wrongMask/' \
        BadWitness.hs >BadMask.hs
    reject BadMask 'The value returned by the interpreter does not match the checked result type.'

    for function in wrongColumns wrongRows wrongCols; do
        sed -e 's/module BadWitness/module BadColumns/' -e "s/Wrong.buildWrong/Wrong.$function/" \
            BadWitness.hs >BadColumns.hs
        reject BadColumns 'The value returned by the interpreter does not match the checked result type.'
    done

    sed -e 's/module ClassEvidence/module NestedField/' -e 's/W.withEvidence/W.nested/' \
        ClassEvidence.hs >NestedField.hs
    reject NestedField 'Cannot reify field 2 of'
    grep -Eq 'multiple hidden indices \((cols, rows|rows, cols)\)' reject.log
    if grep -Fq 'unsupported nested field was evaluated' reject.log; then cat reject.log >&2; exit 1; fi

    sed -e 's/module ClassEvidence/module UnsupportedField/' -e 's/W.withEvidence/W.badField/' \
        ClassEvidence.hs >UnsupportedField.hs
    reject UnsupportedField 'Cannot reify field 4 of'
    grep -Fq 'BadField' reject.log
    if grep -Fq 'unsupported field was evaluated' reject.log; then cat reject.log >&2; exit 1; fi
done
