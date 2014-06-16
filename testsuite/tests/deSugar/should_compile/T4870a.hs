module T4870a where

class C a where c :: a -> a

{-# INLINABLE f #-}
f :: (C a) => a
f = c f

