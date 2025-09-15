{-# LANGUAGE PatternSynonyms, ScopedTypeVariables #-}
module ShouldFail where

pattern Single x = [(x :: Int)]

f :: [Bool] -> Bool
f (Single x) = x
