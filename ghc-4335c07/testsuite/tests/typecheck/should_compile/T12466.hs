{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module T12466 where

class Foo a where
  foo :: (a ~ Int => Int) -> a -> a
  foo _ a2 = a2

instance Foo Char
