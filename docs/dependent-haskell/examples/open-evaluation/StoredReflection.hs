{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module StoredReflection (boxedReflection) where

import Data.Type.Equality ((:~:)(Refl))

-- | A proof at each showable element type.
type Callback = forall a. Show a => foreach (x :: Maybe a) -> x :~: x

-- | Return reflexivity without forcing the index.
reflect :: Callback
reflect _ = Refl

-- | Store the whole polymorphic proof in a Maybe.
boxedReflection :: Maybe Callback
boxedReflection = Just reflect
