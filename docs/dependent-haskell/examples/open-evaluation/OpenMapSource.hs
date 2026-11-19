{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures, TypeApplications #-}
module OpenMapSource (Each) where

import GHC.Exts (FUN, U, Multiplicity(Many))
import qualified OpenMapWorker as W

-- | Use the ordinary mapper with an unmatchable Boolean callback.
type Each :: FUN @U 'Many Bool Bool -> [Bool] -> [Bool]
type Each f xs = W.each f xs
