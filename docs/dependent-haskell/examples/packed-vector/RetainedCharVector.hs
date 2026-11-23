{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
-- | Store a runtime length with a vector of Char at that length.
module RetainedCharVector (CharVector(..), sample, bottomPayload, bottomFields) where

import PackedVecLibrary (Nat(..), Vec(..))

-- | The length is retained. The vector has that length.
data CharVector where
    CharVector :: foreach (n :: Nat) -> Vec Char n -> CharVector

-- | A package containing one character.
sample :: CharVector
sample = CharVector (S Z) (VCons 'r' VNil)

-- | A valid witness with an undefined payload.
bottomPayload :: CharVector
bottomPayload = CharVector (S Z) (error "forced payload")

-- | An undefined witness and an independently undefined payload.
bottomFields :: CharVector
bottomFields = build (error "forced witness")
  where
    build :: foreach (n :: Nat) -> CharVector
    build n = CharVector n (error "forced payload")
