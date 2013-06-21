{-# LANGUAGE TypeFamilies #-}

module Overlap2 where

type family F a b where
  F a a = Int
  F a b = Bool

g :: F Char Double
g = False

h :: F Double Double
h = -2
