-- Pattern synonyms
-- Universially-quantified type variables

{-# LANGUAGE PatternSynonyms #-}
module ShouldCompile where

pattern Single x <- [x]

singleTuple :: [a] -> [b] -> Maybe (a, b)
singleTuple (Single x) (Single y) = Just (x, y)
singleTuple _          _          = Nothing
