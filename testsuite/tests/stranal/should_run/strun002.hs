-- This showed up an "entered-absent-arg" error in 5.02.1

module Main where

is_volatile :: [Int] -> (String,Int) -> Int
is_volatile [] (destVarName, destPtr)
	= error ("Variable not found: " ++ "(" ++ (show destPtr) ++ ") " ++ destVarName)
is_volatile (a:allWrites) (destVarName, destPtr)
  | a == destPtr    = a
  | otherwise = is_volatile allWrites (destVarName, destPtr)

main = print (is_volatile [] ("hello",2))
