{-# LANGUAGE TypeFamilies #-}

module Tc265 where

data T a = MkT (F a)
type family F a where
  F (T a) = a
  F (T Int) = Bool
