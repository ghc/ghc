{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module SumOfSizes (plusExists) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import HaskLib.Size (Size, plus)

-- | Return the sum of two runtime sizes with its equality proof.
plusExists :: foreach (m :: Size) -> foreach (n :: Size)
           -> some (k :: Size) | k :~: plus m n
plusExists m n = Pair (plus m n) Refl
