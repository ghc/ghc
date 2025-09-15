{-# LANGUAGE TypeData #-}
module TDDeriving where

type data Nat = Zero | Succ Nat
    deriving (Eq)
