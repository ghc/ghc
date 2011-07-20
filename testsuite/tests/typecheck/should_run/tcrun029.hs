{-# LANGUAGE DatatypeContexts #-}
-- Killed GHC 5.02.3

-- Confusion about whether the wrapper for a data constructor
-- with a "stupid context" includes the stupid context or not
-- Core lint catches it, but it seg-faults if it runs

module Main where

data Color = Red 
	   | Black
	     deriving Show

data Ord k => Tree k d = None 
		       | Node{color::Color,
			      key::k,
			      item::d,
			      left::(Tree k d),
			      right::(Tree k d)}
			 deriving Show

insert k i t = (insert2 t) {color=Black}
    where insert2 None = Node{color=Red,
			      key=k,
			      item=i,
			      left=None,
			      right=None}

main = print (insert 1 2 None)