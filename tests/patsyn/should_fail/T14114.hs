{-# LANGUAGE PatternSynonyms #-}
module T14114 where

pattern Foo1 a <- (a,a)
pattern Foo2 a  = (a,a)
pattern Foo3 a <- (a,a) where
  Foo3 a = (a,a)
