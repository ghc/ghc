{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}

module Main (main) where

import BoundaryEvidence (Fields(..), fieldAt)
import CheckedOffer (knownHead, labelEight)

-- | Consume the exported paths with ordinary GHC.
main :: IO ()
main = print (fieldAt labelEight fields, fieldAt knownHead fields)
  where
    fields :: Fields '[7, 8] String
    fields = Field "seven" (Field "eight" End)
