
{-# LANGUAGE DuplicateRecordFields #-}

module DupFldFixity3 where

data A = MkA { fld :: A -> A }
data B = MkB { fld :: A -> A }

infixr 4 `fld`
