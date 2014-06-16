-- Pattern synonyms

{-# LANGUAGE PatternSynonyms, GADTs #-}
module ShouldCompile where

data T a where
	MkT :: (Eq b) => a -> b -> T a

pattern P x <- MkT 42 x
