{-# LANGUAGE TypeData, LinearTypes, UnicodeSyntax #-}
module T27732f where

type data T where
  MkT :: Int ⊸ T
