{-# LANGUAGE OrPatterns, PatternSynonyms #-}

module Main where

main = case 3 of
  (one of 4) -> False

g x = case x of
  one of 4, 5 -> False
