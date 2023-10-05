

{-# LANGUAGE DuplicateRecordFields #-}

module DupFldFixity1 where

data A = MkA { fld :: A -> A }

infixr 4 `fld`
