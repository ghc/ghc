{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Foo (A(P,Q)) where

data A a = A a

pattern P :: Show a => a -> A a
pattern P a = A a

pattern Q :: (A ~ f) => a -> f a
pattern Q a = A a
