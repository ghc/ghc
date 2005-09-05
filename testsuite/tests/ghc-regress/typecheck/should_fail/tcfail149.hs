module Main where

class C a where
  op :: (Show a, Show b) => a -> b -> String
	-- This class op adds a constraint on 'a'

