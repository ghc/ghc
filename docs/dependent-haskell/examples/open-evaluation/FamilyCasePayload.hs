{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
module FamilyCasePayload (ChoosePayload, KeepPayload, Dispatch, Count(..), Down) where

import Data.Kind (Type)

type family ChoosePayload (fallback :: Type) (input :: Maybe Type) :: Type where
  forall fallback (input :: Maybe Type).
    ChoosePayload fallback input = case input of
      Nothing -> fallback
      Just field -> field

type family KeepPayload (flag :: Bool) (payload :: Type) :: Type where
  KeepPayload flag payload = let chosen = flag in case chosen of
    True -> Maybe payload
    False -> [payload]

type family Dispatch (flag :: Bool) (input :: Maybe Type) :: Type where
  Dispatch 'True input = case input of
    Nothing -> Bool
    Just field -> field
  Dispatch 'False input = case input of
    Nothing -> Char
    Just field -> Maybe field

data Count = Z | S Count

type family Down (count :: Count) :: Bool where
  Down 'Z = 'False
  Down ('S count) = case count of
    Z -> 'True
    S prior -> Down ('S prior)
