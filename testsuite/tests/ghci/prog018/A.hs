{-# OPTIONS_GHC -Wincomplete-patterns -Wunused-matches #-}
module A where

incompletePattern :: Int -> Int
incompletePattern 0 = 0

unusedMatches :: Int -> Int
unusedMatches x = 0
