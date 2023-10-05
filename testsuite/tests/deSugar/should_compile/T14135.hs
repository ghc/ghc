{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE GADTs #-}
module T14135 where

data Foo a where
  Foo1 :: a -> Foo a
  Foo2 :: Int -> Foo Int

pattern MyFoo2 :: (a ~ Int) => Int -> Foo a
pattern MyFoo2 i = Foo2 i

{-# COMPLETE Foo1, MyFoo2 #-}

f :: Foo a -> a
f (Foo1 x) = x
