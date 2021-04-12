{-# LANGUAGE TypeFamilies #-}

module T19682b where

type family Arg x where
  Arg (a -> b) = a

type family Res x where
  Res (a -> b) = b

class C a
instance C (a -> b)

f :: (C x, x ~ (Arg x -> Res x)) => x
f = undefined

g = f
