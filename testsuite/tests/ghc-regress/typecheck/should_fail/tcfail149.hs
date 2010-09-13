module Main where

class C a where
  op :: (Show a, Show b) => a -> b -> String
	-- This class op adds a constraint on 'a'

	-- In GHC 7.0 this is fine, and it's a royal 
	-- pain to reject it when in H98 mode, so
	-- I'm just allowing it

instance C Int where
  op x y = show x ++ " " ++ show y

main = print (op (1::Int) 2)
