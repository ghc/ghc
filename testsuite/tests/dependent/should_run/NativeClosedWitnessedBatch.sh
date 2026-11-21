#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -fdiagnostics-color=never -dcore-lint -dstg-lint)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
producer_flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
if "${ghc[@]}" --info | grep -F '("Support dynamic-too","YES")' >/dev/null; then
    producer_flags+=(-dynamic-too)
fi

for opt in -O0 -O2; do
    mkdir -p "$out/${opt#-}"/{producer,client}
    cd "$out/${opt#-}/producer"
    cp "$src"/{ExistentialVec,IndexedVec,WitnessedBatch}.hs .
    cat >WrongWitness.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module WrongWitness
    ( buildWrong, EvidenceBatch(..), withEvidence, TaggedBatch(..), withEquality
    ) where
import ExistentialVec (SN(..))
import IndexedVec (Vec(..))
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)
import WitnessedBatch (Batch(..))

buildWrong :: Natural -> Batch Natural
buildWrong n = Chunk (SS SZ) (VCons n VNil)
                     (Chunk (SS SZ) (unsafeCoerce (VCons n (VCons n VNil))) End)

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
module Proof (evenBatch, oddBatch) where

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
HS
    cat >Demand.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Demand (firstWidth) where
import Data.Type.Equality ((:~:)(Refl))
import qualified WitnessedBatch as B (firstWidth, partial)

firstWidth :: B.firstWidth (B.partial 9) :~: 1
firstWidth = Refl
HS
    cat >Main.hs <<'HS'
module Main where
import qualified Demand
import Proof (evenBatch, oddBatch)
import qualified WitnessedBatch as B

main :: IO ()
main = print (evenBatch, oddBatch, Demand.firstWidth,
              B.firstWidth (B.build 4), B.firstWidth (B.build 3), B.firstWidth (B.partial 9))
HS
    "${ghc[@]}" "$opt" -c Proof.hs Demand.hs
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" ./*.o -o run
    test "$(./run)" = '(Refl,Refl,Refl,1,2,1)'

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
done
