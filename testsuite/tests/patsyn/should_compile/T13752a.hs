{-# LANGUAGE PatternSynonyms, RankNTypes #-}

module T13752a where

data T = MkT (forall a. a->a)

pattern P :: (Int -> Int) -> T
pattern P x <- MkT x
