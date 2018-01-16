{-# LANGUAGE PatternSynonyms, TypeFamilies #-}
module T11039 where

data A a = A a

-- This should fail
pattern Q :: () => (A ~ f) => a -> f a
pattern Q a = A a
