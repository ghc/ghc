{-# LANGUAGE TypeFamilies #-}

module Overlap11 where

type family F a b
type instance where
  F a a = Int
  F a b = b

g :: F a Int
g = (5 :: Int)




