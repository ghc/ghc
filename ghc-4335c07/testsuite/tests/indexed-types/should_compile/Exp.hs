{-# LANGUAGE TypeFamilies #-}

module Exp (C, C(type T), T, foo, S)
where

class C a where
  data T a :: *
  foo :: a -> a

data family S a b :: *
