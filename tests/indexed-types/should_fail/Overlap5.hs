{-# LANGUAGE TypeFamilies #-}

module Overlap5 where

type family G a
type family F a where
  F Int = Bool
  G Int = Char
  F a = Int