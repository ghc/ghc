module Expression.Simplify (
    Simplify (..)
    ) where

class Simplify a where
    simplify :: a -> a
