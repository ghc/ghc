{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes, PolyKinds, DataKinds #-}

module SAKS_010 where

import Data.Kind (Type)

type W :: forall (a :: forall k. k -> Type) -> a Int -> a Maybe -> Type
data W x (y :: x Int) (z :: x Maybe)
