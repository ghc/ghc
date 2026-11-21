{-# LANGUAGE DataKinds, GADTs, Modifiers, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
module NativeMatchableProjectionLib (project) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M)

-- | Both heads are statically matchable in this declaration.
project :: forall (f :: Type %M -> Type) x.
           f x :~: Maybe Char -> (f Int -> f Bool) -> Maybe Int -> Maybe Bool
project Refl run = run
{-# INLINE project #-}
