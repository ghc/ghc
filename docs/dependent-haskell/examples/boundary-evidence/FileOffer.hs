{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}

module FileOffer (labelEight, main) where

import BoundaryEvidence (Fields (..), Member, fieldAt)
import EmitMembership (emitFilePosition)
import qualified OrdinaryOffer

-- | Check the proposed position against label eight in this producer result.
labelEight :: Member 8 (OrdinaryOffer.offered 7)
labelEight = $(emitFilePosition "offer.position" (OrdinaryOffer.offered 7))

-- | Show which field the checked position selects.
main :: IO ()
main = putStrLn (fieldAt labelEight (Field "first" (Field "second" End)))
