{-# LANGUAGE TypeFamilies #-}

module Overlap10 where

type family F a b where
  F a a = Int
  F a b = b

g :: a -> F a Bool
g x = False




