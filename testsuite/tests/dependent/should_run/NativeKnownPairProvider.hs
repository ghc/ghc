{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}

module NativeKnownPairProvider (Named, Open, Payload (..)) where

import Data.Kind (Type)

data Payload (tag :: Bool) where
    Yes :: Int -> Payload True
    No :: Int -> Payload False

type Named = some (tag :: Bool) | Payload tag

type family Open a :: Type
type instance Open Bool = Named
type instance Open Ordering = Named
