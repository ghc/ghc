{-# LANGUAGE TypeData #-}
module TDPattern where

type data Nat = Zero | Succ Nat

-- Zero is not a data constructor
f Zero = 0
