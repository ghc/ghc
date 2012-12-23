{-# LANGUAGE TypeFamilies #-}

module Overlap10 where

type family F a b
type instance where
  F a a = Int
  F a b = b

g :: F a Bool
g = False




