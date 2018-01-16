{-# LANGUAGE TypeFamilies #-}
module T8979 where

type family F a
type family G a

type H a = G a

f :: F (G Char) -> F (H Char)
f a = a
