
{-# LANGUAGE NoFieldSelectors #-}

module DupFldFixity2 where

data A = MkA { fld :: A -> A }
data B

fld :: B -> B -> B
fld x _ = x

infixr 4 `fld`
