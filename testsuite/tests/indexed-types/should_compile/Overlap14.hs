{-# LANGUAGE TypeFamilies #-}

module Overlap14 where

type family F a b c where
  F a a a   = Int
  F Int b c = Bool

type family G x

foo :: F Int (G Bool) Bool
foo = False
