

module Main where

class C a where
  op :: (Show a, Show b) => a -> b -> String
	-- This class op has local quantification, but
	-- also adds a constraint on 'a'

instance C Bool where
  op x y = show x ++ " " ++ show y

main = do { putStrLn (op True 'x'); putStrLn (op False (3::Int)) } 
