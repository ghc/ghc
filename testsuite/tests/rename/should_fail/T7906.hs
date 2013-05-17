module T7906 where

class Foo f where
    foo :: f
{-# INLINEABLE foo #-}
