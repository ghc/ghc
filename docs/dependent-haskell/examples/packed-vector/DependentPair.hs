{-# LANGUAGE ExperimentalDependentTypes, GADTs, PolyKinds, RankNTypes #-}
-- | Store a witness and a payload indexed by that witness.
module DependentPair (DPair(..), first, withPair) where

import Data.Kind (Type)

-- | The witness type and the payload family are both parameters.
data DPair (a :: Type) (p :: a -> Type) where
    Pair :: foreach (x :: a) -> p x -> DPair a p

-- | Return the witness without forcing the payload.
first :: DPair a p -> a
first (Pair x _) = x

-- | Open the package for a consumer that accepts every stored index.
withPair :: DPair a p -> (foreach (x :: a) -> p x -> r) -> r
withPair (Pair x value) consume = consume x value
