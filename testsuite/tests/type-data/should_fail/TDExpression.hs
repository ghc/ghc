{-# LANGUAGE TypeData #-}
module TDExpression where

type data Nat = Zero | Succ Nat

-- Not in scope: data constructor ‘Zero’
z = Zero
