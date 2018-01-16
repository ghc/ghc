{-# LANGUAGE PolyKinds #-}
module T10516 where

type App f a = f a

newtype X f a = X (f a)

f :: f a -> X (App f) a
f = X
