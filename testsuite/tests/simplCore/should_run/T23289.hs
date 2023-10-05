{-# LANGUAGE ImplicitParams, GADTs #-}
module Main where

data T where
  MkT :: (?f :: Int) => T

f :: T -> T -> Int
f MkT MkT = ?f

main :: IO ()
main = do
  print (let ?g = 1 in let ?g = 2 in ?g)
  print $ f (let ?f = 3 in MkT) (let ?f = 4 in MkT)
