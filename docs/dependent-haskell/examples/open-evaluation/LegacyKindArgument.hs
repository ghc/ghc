{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, TypeFamilies #-}

module LegacyKindArgument (maybeField, defaultCallback) where

import Data.Kind (Type)
import DefaultCallback (Callback)

-- | Store a value with its constructor as a type index.
data Field :: forall (k :: Type) -> k -> Type where
    MaybeField :: Maybe Bool -> Field (Type -> Type) Maybe

-- | Use the matchable arrow required by the second argument.
type Optional = Field (Type -> Type) Maybe

-- | Use the ordinary Maybe constructor to determine the arrow in its index.
maybeField :: Maybe Bool -> Maybe Bool
maybeField x = case (MaybeField x :: Optional) of
    MaybeField y -> y

-- | Apply a promoted constructor at the arrow from the class default.
type family ApplyDefault (f :: Callback Bool) :: Type
type instance ApplyDefault 'Just = Maybe Bool

-- | Use the same default as the type of an ordinary function.
callback :: Callback Bool
callback b = Just (not b)

-- | Check the computed result and run the callback.
defaultCallback :: ApplyDefault 'Just
defaultCallback = callback False
