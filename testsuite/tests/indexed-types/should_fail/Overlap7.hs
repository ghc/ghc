{-# LANGUAGE TypeFamilies #-}

module Overlap7 where

type family F a b where
  F Int a = Int
  F a b = Bool
type instance F a Int = Int

