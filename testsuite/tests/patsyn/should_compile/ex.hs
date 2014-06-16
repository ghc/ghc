-- Pattern synonyms
-- Existentially-quantified type variables

{-# LANGUAGE GADTs, PatternSynonyms #-}
module ShouldCompile where

data T where
    MkT :: b -> (b -> Bool) -> T

pattern P x f <- MkT x f

test :: T -> Bool
test (P x f) = f x
