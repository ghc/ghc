{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module T12918b where

class Foo1 a where
  -- These ones should be rejected
  bar1         :: a -> b
  default bar1 :: b -> a
  bar1 = undefined

  bar2         :: a -> b
  default bar2 :: x
  bar2 = undefined

  bar3         :: a -> b
  default bar3 :: a -> Int
  bar3 = undefined

  bar4         :: a -> Int
  default bar4 :: a -> b
  bar4 = undefined

  -- These ones are OK
  baz1         :: forall b c. a -> b -> c
  default baz1 :: forall b c. a -> b -> c
  baz1 = undefined

  baz2         :: forall b c. a -> b -> c
  default baz2 :: forall c b. a -> b -> c
  baz2 = undefined

  baz3         :: a -> b -> c
  default baz3 :: a -> x -> y
  baz3 = undefined
