module T15445a where

{-# SPECIALIZE plusTwoRec :: [Int] -> [Int] #-}
plusTwoRec :: Num a => [a] -> [a]
plusTwoRec [] = []
plusTwoRec (x:xs) = x+2:plusTwoRec xs

plusTwoRec' :: Num a => [a] -> [a]
plusTwoRec' [] = []
plusTwoRec' (x:xs) = x+2:plusTwoRec' xs
