{-# LANGUAGE TypeFamilies #-}

module Overlap8 where

type family F a b
type instance F a Int = Int
type instance where
  F Int a = Int
  F a b = Bool


