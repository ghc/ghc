module Main where

import T14955a

--test1 :: [Bool] -> Bool
--test1 = ors

--test2 :: [Bool] -> Bool
--test2 = dors boolDict

--test2a :: [Bool] -> Bool
--test2a xs = dors boolDict xs

test3 :: [Bool] -> Bool
test3 xs = pors xs

--test4 :: [Bool] -> Bool
--test4 xs = porsProxy xs

main = print (test3 (replicate 1000000 False))
