{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds #-}

module LegacyKindArgument (maybeField) where

import Data.Kind (Type)

-- | Store a value with its constructor as a type index.
data Field :: forall (k :: Type) -> k -> Type where
    MaybeField :: Maybe Bool -> Field (Type -> Type) Maybe

-- | Use the matchable arrow required by the second argument.
type Optional = Field (Type -> Type) Maybe

-- | Use the ordinary Maybe constructor to determine the arrow in its index.
maybeField :: Maybe Bool -> Maybe Bool
maybeField x = case (MaybeField x :: Optional) of
    MaybeField y -> y
