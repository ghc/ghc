{-# LANGUAGE PatternSynonyms, TypeFamilies #-}
module T11039a where

data A a = A a

-- This should succeed
pattern Q2 :: (A ~ f) => a -> f a
pattern Q2 a = A a
