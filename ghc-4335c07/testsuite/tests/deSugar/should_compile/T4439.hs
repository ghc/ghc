{-# LANGUAGE ViewPatterns, ExistentialQuantification #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- None of these should give incomplete-pattern warnings

module T4439 where

data Moo = Moo (Char -> Int)
spqr (Moo _) = undefined      
foo (id -> Moo _) = undefined 


data Exists = forall a. Exists (a -> Int)
bar (Exists _) = undefined       
baz (id -> Exists _) = undefined 
