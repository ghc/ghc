module T10844a where

class Foo a where foo :: a -> a

instance Foo Int where
    foo 0 = error "foo"
    foo n = n
    {-# INLINE foo #-}
