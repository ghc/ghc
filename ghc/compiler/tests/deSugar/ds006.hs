--!!! ds006 -- v | True = v+1 | False = v (dead code elim)
--
module Test where

v | True  = v + 1
  | False = v
