{-# LANGUAGE TypeFamilies #-}
module T13420 where

type family F a where
  F [Int] = Bool
  F [a]   = Double
  F (a b) = Char
