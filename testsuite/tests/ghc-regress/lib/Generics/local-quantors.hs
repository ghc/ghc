{-# OPTIONS -fglasgow-exts #-}

-- A datatype with a locally quantified component
-- Seems to be too polymorphic to descend into structure!
-- Largely irrelevant?!

module Main where
import Data.Generics

data Test = Test (GenericT) deriving Typeable

instance Data Test
  where
    conOf (Test _) = Constr "Test"
    consOf _ = [Constr "Test"]
    gfoldl f z x
      = z x -- folding without descent 
    gunfold f z c | conString c == "Test"
      = undefined -- unfolding omitted

-- Test for compilation only
main = undefined
