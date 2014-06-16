-- Pattern synonyms

{-# LANGUAGE PatternSynonyms, GADTs #-}
module ShouldCompile where

data T a where
	MkT :: (Eq b) => a -> b -> T a

pattern P x y <- MkT x y

f :: T Bool -> Bool
f (P x y) = x && y == y
