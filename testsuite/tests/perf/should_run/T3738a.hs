
module T3738a where

{-# INLINE foo #-}
foo :: Num a => a -> [a]
foo x = map (+ 1) (repeat x)
