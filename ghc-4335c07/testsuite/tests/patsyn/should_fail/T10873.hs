{-# LANGUAGE PatternSynonyms, GADTs #-}

module T10873 where

pattern Pat1 :: () => Show a => a -> Maybe a
pattern Pat1 x <- Just x

data T a where MkT :: (Ord a) => a -> T a
pattern Pat2 :: (Enum a) => Show a => a -> T a
pattern Pat2 x <- MkT x
