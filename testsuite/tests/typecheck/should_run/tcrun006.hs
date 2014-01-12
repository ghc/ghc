{-# LANGUAGE DatatypeContexts #-}
-- !!! Selectors for data and newtypes with contexts

-- This program, reported in Aug'00 by Jose Emilio Labra Gayo
-- gave rise to a Lint error because the selector 'newout' below
-- was given the type
--	Eq f => NewT f -> f
-- but lacked a dictionary argument in its body.

module Main where

newtype (Eq f) => NewT  f = NewIn  { newout  :: f } 
data    (Eq f) => DataT f = DataIn { dataout :: f } 

main = print (newout (NewIn "ok new") ++ dataout (DataIn " ok data"))

