{-# LANGUAGE PatternSynonyms #-}
module ShouldCompile where

pattern SinglePair :: (a, a) -> [(a, a)]
pattern SinglePair x = [x]

f :: (Show a) => [(a, a)] -> String
f (SinglePair x) = show x
