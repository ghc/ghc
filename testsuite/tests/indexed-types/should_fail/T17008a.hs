{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T17006 where

import Data.Kind

type family ConstType (a :: Type) :: Type where
  ConstType _ = Type

type family F (x :: ConstType a) :: Type where
  F (x :: ConstType a) = a

f :: F Int
f = let x = x in x
