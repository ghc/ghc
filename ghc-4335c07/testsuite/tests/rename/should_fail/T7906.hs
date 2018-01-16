module T7906 where

class Foo f where
    foo :: f
{-# INLINABLE foo #-}
