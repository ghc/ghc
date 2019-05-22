module T16610 where

data Foo = Foo
instance Eq Foo where
  {-# INLINE wrong #-}
  wrong _ = True
