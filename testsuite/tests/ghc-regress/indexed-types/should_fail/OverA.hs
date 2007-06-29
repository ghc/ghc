{-# OPTIONS -ftype-families -fglasgow-exts #-}

module OverA (C, D)
where

data family C a b :: *

type family D a b :: *