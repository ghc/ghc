{-# LANGUAGE Haskell2010 #-}
module T15527 where

f :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
f =  (.) @Int
