{-# LANGUAGE
    ScopedTypeVariables
  , DataKinds
  , GADTs
  , RankNTypes
  , TypeOperators
  , PolyKinds -- Comment out PolyKinds and the bug goes away.
  #-}
{-# OPTIONS_GHC -O #-}
  -- The bug is in GHC.Core.Opt.Simplify.Utils.abstractFloats, so we need -O to trigger it

module KeyValue where

import Data.Kind

data AccValidation err a = AccFailure err | AccSuccess a

data KeyValueError = MissingValue

type WithKeyValueError = AccValidation [KeyValueError]

missing :: forall f rs. RecApplicative rs => Rec (WithKeyValueError :. f) rs
missing = rpure missingField
  where
    missingField :: forall x. (WithKeyValueError :. f) x
    missingField = Compose $ AccFailure [MissingValue]

data Rec :: (u -> Type) -> [u] -> Type where
  RNil :: Rec f '[]
  (:&) :: !(f r) -> !(Rec f rs) -> Rec f (r ': rs)

newtype Compose (f :: l -> Type) (g :: k -> l) (x :: k)
  = Compose { getCompose :: f (g x) }

type (:.) f g = Compose f g

class RecApplicative rs where
  rpure
    :: (forall x. f x)
    -> Rec f rs
