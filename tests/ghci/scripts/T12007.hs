{-# LANGUAGE PatternSynonyms #-}
module T12007 where

data Foo a = Foo a a

pattern A a1 a2 = Foo a1 a2
pattern B a1 a2 = A a1 a2
