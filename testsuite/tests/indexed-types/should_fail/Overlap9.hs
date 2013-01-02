{-# LANGUAGE TypeFamilies #-}

module Overlap9 where

type family F a
type instance where
  F Int = Bool
  F a   = Int

g :: Show a => a -> F a
g x = length (show x)



