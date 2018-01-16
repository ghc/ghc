-- Test that a COMPLETE pragma over constructors of different types fails.
module TyMismatch where

data E = L | R

{-# COMPLETE Just, L #-}
