{-# LANGUAGE ImplicitParams, RankNTypes #-}

module Main where

f :: (?x :: Int) => ((?x :: Int) => Int) -> Bool -> Int  
f g False = g  
f g True = let ?x = ?x + 1
           in f g False

h :: (?x :: Int) => Int
h = ?x

main :: IO ()
main = print (let ?x = 0 in f h True)
