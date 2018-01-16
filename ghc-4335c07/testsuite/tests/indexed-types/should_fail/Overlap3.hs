{-# LANGUAGE TypeFamilies #-}

module Overlap3 where

type family F a b where
  F a a = Int
  F a b = Bool
type instance F Char Char = Int

g :: F Char Double
g = False

h :: F Double Double
h = -2
