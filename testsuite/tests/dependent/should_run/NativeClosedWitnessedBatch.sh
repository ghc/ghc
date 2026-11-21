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
    cp "$src"/{ExistentialVec,IndexedVec,WitnessedBatch,RectangularTable}.hs .
    cat >WrongWitness.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module WrongWitness
    ( buildWrong, wrongMask, wrongColumns, BadField(..), badField
    , wrongRows, wrongCols, wrongCellRows, wrongCellCols, wrongUnlabelled, Nested(..), nested
    , noRows, noCols, LabelledLast(..), labelsLast, wrongLast
    , EvidenceBatch(..), withEvidence, TaggedBatch(..), withEquality
    ) where
import Data.Proxy (Proxy)
import ExistentialVec (SN(..))
import IndexedVec (N(..), Vec(..))
import Numeric.Natural (Natural)
import RectangularTable (Table(..), ColumnTable(..), sample)
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

wrongCellRows, wrongCellCols :: Natural -> Table Natural
wrongCellRows n = case sample n of
    Table rows cols _ -> Table rows cols (unsafeCoerce (VNil :: Vec 'Z (Vec 'Z Natural)))
wrongCellCols n = case sample n of
    Table rows cols (VCons row (VCons _ rest)) ->
        Table rows cols (VCons row (VCons (unsafeCoerce (VNil :: Vec 'Z Natural)) rest))
    table -> table

noRows, noCols :: Char -> Table Natural
noRows col = Table VNil (VCons col VNil) VNil
noCols row = Table (VCons row VNil) VNil (VCons VNil VNil)

wrongUnlabelled :: Natural -> ColumnTable Natural
wrongUnlabelled n = case sample n of
    Table _ cols _ -> ColumnTable cols (VCons (unsafeCoerce VNil) VNil)

data LabelledLast a where
    LabelledLast :: Vec cols Char -> Vec rows (Vec cols a) -> Vec rows Char -> LabelledLast a

labelsLast, wrongLast :: Natural -> LabelledLast Natural
labelsLast n = case sample n of
    Table rows cols cells -> LabelledLast cols cells rows
wrongLast n = case sample n of
    Table _ cols cells -> LabelledLast cols cells (unsafeCoerce VNil)

data Nested a where
    Nested :: Char -> Vec rows (Vec cols a) -> Nested a

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
    "${ghc[@]}" "$opt" "${producer_flags[@]}" -c IndexedVec.hs ExistentialVec.hs WitnessedBatch.hs RectangularTable.hs WrongWitness.hs
    cd "../client"
    cp ../producer/*.{hi,o} .
    test ! -e WitnessedBatch.hs
    test ! -e RectangularTable.hs
    if [[ -f ../producer/WitnessedBatch.dyn_o ]]; then cp ../producer/*.{dyn_hi,dyn_o} .; fi
    cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module Proof (evenBatch, oddBatch, mask, empty, row, margins, table, emptyRows, emptyCols, totals) where

import Data.Type.Equality ((:~:)(Refl))
import qualified ExistentialVec as E
import qualified IndexedVec as V
import qualified WitnessedBatch as B
import qualified RectangularTable as T
import qualified WrongWitness as W

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

type Two x y = 'V.VCons x ('V.VCons y 'V.VNil)
type Three x y z = 'V.VCons x (Two y z)

table :: T.sample 4 :~: 'T.Table (Two 'r' 's') (Three 'x' 'y' 'z')
                                (Two (Three 1 2 3) (Three 4 5 6))
table = Refl

unlabelled :: T.withoutRowLabels (T.sample 4) :~: 'T.ColumnTable
                (Three 'x' 'y' 'z') (Two (Three 1 2 3) (Three 4 5 6))
unlabelled = Refl

labelsAfterCells :: W.labelsLast 4 :~: 'W.LabelledLast
                    (Three 'x' 'y' 'z') (Two (Three 1 2 3) (Three 4 5 6)) (Two 'r' 's')
labelsAfterCells = Refl

unlabelledEmpty :: T.withoutRowLabels (W.noRows 'z') :~: 'T.ColumnTable ('V.VCons 'z' 'V.VNil) 'V.VNil
unlabelledEmpty = Refl

unlabelledNoCols :: T.withoutRowLabels (W.noCols 's') :~: 'T.ColumnTable 'V.VNil ('V.VCons 'V.VNil 'V.VNil)
unlabelledNoCols = Refl

emptyRows :: W.noRows 'z' :~: 'T.Table 'V.VNil ('V.VCons 'z' 'V.VNil) 'V.VNil
emptyRows = Refl

emptyCols :: W.noCols 's' :~: 'T.Table ('V.VCons 's' 'V.VNil) 'V.VNil ('V.VCons 'V.VNil 'V.VNil)
emptyCols = Refl

totals :: T.totals (T.sample 4) :~: 'B.TableMargins (Two 'r' 's') (Three 'x' 'y' 'z')
                                                   (Two 6 15) (Three 5 7 9)
totals = Refl
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
import Proof (evenBatch, oddBatch, mask, row, empty, margins, table, emptyRows, emptyCols, totals)
import qualified RectangularTable as T
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
    let noRows = T.Table V.VNil (V.VCons 'z' V.VNil) V.VNil
        noCols = T.Table (V.VCons 's' V.VNil) V.VNil (V.VCons V.VNil V.VNil)
    print (table, emptyRows, emptyCols, totals,
           T.selectedTotals (T.sample 7), T.selectedTotals noRows, T.selectedTotals noCols)
HS
    "${ghc[@]}" "$opt" -c Proof.hs Demand.hs
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" ./*.o -o run
    test "$(./run)" = $'(Refl,Refl,Refl,Refl,Refl,1,2,1,[5],[])\n(Refl,Refl,Just 5,Just 8)\n(Refl,(Just 8,Just 4),Just 7)\n(Refl,Refl,Refl,Refl,(Just 18,Just 9),(Nothing,Just 0),(Just 0,Nothing))'

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

    cat >Ragged.hs <<'HS'
module Ragged where
import IndexedVec (Vec(..))
import RectangularTable (Table(..))
bad :: Table Int
bad = Table (VCons 'r' VNil) (VCons 'x' (VCons 'y' VNil))
            (VCons (VCons 1 VNil) VNil)
HS
    if "${ghc[@]}" "$opt" -c Ragged.hs >ragged.log 2>&1 ||
       ! grep -Fq 'Ragged.hs:6:29: error: [GHC-83865]' ragged.log; then
        cat ragged.log >&2
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

    for function in wrongLast wrongUnlabelled wrongCellRows wrongCellCols; do
        sed -e 's/module BadWitness/module BadCells/' -e "s/Wrong.buildWrong/Wrong.$function/" \
            -e '/^import qualified WrongWitness/i import RectangularTable (Table(..), ColumnTable(..))' \
            BadWitness.hs >BadCells.hs
        reject BadCells 'The value returned by the interpreter does not match the checked result type.'
    done

    sed -e 's/module BadCells/module HiddenTable/' -e '/^import RectangularTable/d' BadCells.hs >HiddenTable.hs
    reject HiddenTable 'Cannot inspect the constructors of RectangularTable.Table.'

    sed -e 's/module ClassEvidence/module NestedField/' -e 's/W.withEvidence/W.nested/' \
        ClassEvidence.hs >NestedField.hs
    reject NestedField 'Cannot reify field 2 of'
    grep -Eq 'multiple hidden indices \((cols, rows|rows, cols)\)' reject.log
    grep -Fq 'Earlier fields must determine the other indices.' reject.log
    if grep -Fq 'unsupported nested field was evaluated' reject.log; then cat reject.log >&2; exit 1; fi

    sed -e 's/module ClassEvidence/module UnsupportedField/' -e 's/W.withEvidence/W.badField/' \
        ClassEvidence.hs >UnsupportedField.hs
    reject UnsupportedField 'Cannot reify field 4 of'
    grep -Fq 'BadField' reject.log
    if grep -Fq 'unsupported field was evaluated' reject.log; then cat reject.log >&2; exit 1; fi
done
