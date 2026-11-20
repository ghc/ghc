{-# LANGUAGE ExperimentalDependentTypes, ImpredicativeTypes, RankNTypes, TypeApplications #-}

module RankedPackage (runCallback) where

import Data.Kind (Type)

data Callback t = Callback t (t -> (Int, Bool))
type Hidden = exists (t :: Type). Callback t
type PolyId = forall a. a -> a

-- | Store one polymorphic identity function and use it at two types.
hidden :: Hidden
hidden = Callback @PolyId id (\f -> (f 7, f True))

-- | Open the package and give its value to the matching callback.
runCallback :: (Int, Bool)
runCallback = case hidden of Callback value call -> call value
