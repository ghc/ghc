{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}
module RetainedProjection (project) where

import RetainedNat (Retained(..))
import qualified RetainedWitness

-- | Return the payload at this package's stored index.
project :: foreach (q :: Retained p) -> p (RetainedWitness.witness q)
project (Retain _ value) = value
