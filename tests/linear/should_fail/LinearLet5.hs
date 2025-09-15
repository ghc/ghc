{-# LANGUAGE LinearTypes #-}
module LinearLet5 where

f :: a -> (a, a)
f x = let %1 y = x ; %1 z = y in (z, y)
