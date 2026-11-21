{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}

module NativeOffer (Result, choose) where

import BoundaryEvidence (Fields (..), Member, fieldAt)
import EmitMembership (emitFilePosition)
import qualified OrdinaryOffer

-- | Check the position against the exact producer input and label.
labelEight :: Member 8 (OrdinaryOffer.offered 7)
labelEight = $(emitFilePosition "offer.position" (OrdinaryOffer.offered 7))

-- | Select the result type at the retained Boolean index.
type Result (b :: Bool) = case b of { True -> String; False -> Bool }

-- | Use replayed membership in a branch of a native retained function.
choose :: foreach (b :: Bool) -> Result b
choose True = fieldAt labelEight (Field "seven" (Field "eight" End))
choose False = True
