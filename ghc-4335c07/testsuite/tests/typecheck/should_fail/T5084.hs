module T5084 where

-- Superclass method pragma test (fail)
class Foo a where
    bar :: a -> a
    {-# INLINE bar #-}

-- Instance test (ok)
instance Foo Int where
    bar = (+1)
    {-# INLINE bar #-}

