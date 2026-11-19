{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}

module SplicedOffer (labelEight, main) where

import BoundaryEvidence (Fields (..), Member, fieldAt)
import EmitMembership (emitMembership)
import qualified OrdinaryOffer

-- | GHC checks the generated path against the actual producer at input seven.
labelEight :: Member 8 (OrdinaryOffer.offered 7)
labelEight = $(emitMembership 8 (OrdinaryOffer.offered 7))

-- | Show which of the two fields has label eight.
main :: IO ()
main = putStrLn (fieldAt labelEight (Field "first" (Field "second" End)))
