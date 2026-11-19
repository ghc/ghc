{-# LANGUAGE NoExperimentalDependentTypes, TemplateHaskell #-}

module EmitMembership (emitMembership) where

import BoundaryEvidence (Member (..))
import Language.Haskell.TH (Exp, Q)
import Numeric.Natural (Natural)

-- | Search an ordinary list at compile time. Emit a path for GHC to check.
emitMembership :: Natural -> [Natural] -> Q Exp
emitMembership target = search
  where
    search [] = fail ("The offer has no field labelled " ++ show target ++ ".")
    search (label : rest)
        | label == target = [|Here|]
        | otherwise = [|There $(search rest)|]
