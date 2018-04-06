{-# LANGUAGE UnboxedSums #-}

module Main where

import T14051a

main :: IO ()
main = print $ case func () of
  (# True | #) -> 123
  _ -> 321
