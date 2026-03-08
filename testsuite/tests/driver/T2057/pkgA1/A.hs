{-# LANGUAGE NoImplicitPrelude #-}
module A (T(..), f) where

data T = MkT

f :: T -> T
f x = x
