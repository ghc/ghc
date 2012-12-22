{-# LANGUAGE TypeFamilies #-}

module Overlap1 where

type family F a
type instance where
  F Int = Int
  F a = Bool

g :: F Int
g = 5

h :: F Char
h = False
