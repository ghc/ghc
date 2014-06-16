{-# LANGUAGE TypeFamilies #-}

module Overlap13 where

type family F a b where
  F a a = Int
  F a Int = Int
  F a b = b

g :: a -> F a Int
g x = (5 :: Int)




