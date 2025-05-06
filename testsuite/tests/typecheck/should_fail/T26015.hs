{-# LANGUAGE MonoLocalBinds, GADTs, TypeFamilies #-}

module Foo where

type family F a
type instance F Int = Bool

data T where
  T1 :: a -> T
  T2 :: Int -> T

woo :: (a ~ Int) => Int -> F a
woo = error "urk"

f x = case x of
         T1 y -> not (woo 3)
         T2 -> True
