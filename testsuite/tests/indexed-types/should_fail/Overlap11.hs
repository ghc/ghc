{-# LANGUAGE TypeFamilies #-}

module Overlap11 where

type family F a b
type instance where
  F a a = Int
  F a b = b

g :: a -> F a Int
g x = (5 :: Int)




