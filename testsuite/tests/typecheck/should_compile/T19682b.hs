{-# LANGUAGE TypeFamilies #-}

module T19682b where

import Data.Kind

type family Arg x where
  Arg ((a :: Type) -> b) = a

type family Res x where
  Res (a -> (b :: Type)) = b

class C a
instance C (a -> b)

f :: (C x, x ~ (Arg x -> Res x)) => x
f = undefined

g = f
