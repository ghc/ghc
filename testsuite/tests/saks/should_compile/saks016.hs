{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds, RankNTypes #-}

module SAKS_016 where

import Data.Kind (Type)

type T :: forall k. k -> forall j. j -> Type
data T (x :: hk) (y :: hj)
