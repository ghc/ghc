{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import OpenConstructorSource (left, nested, right)

main :: IO ()
main = print (left True 'x', right 'y' False, nested (7 :: Int) 'z')
