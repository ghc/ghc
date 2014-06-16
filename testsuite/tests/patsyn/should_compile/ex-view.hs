-- Pattern synonyms

{-# LANGUAGE PatternSynonyms, GADTs, ViewPatterns #-}
module ShouldCompile where

data T a where
	MkT :: (Eq b) => a -> b -> T a

f :: (Show a) => a -> Bool
f = undefined

pattern P x <- MkT (f -> True) x
