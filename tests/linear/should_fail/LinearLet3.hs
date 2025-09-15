{-# LANGUAGE LinearTypes #-}
module LinearLet3 where

f :: a -> (a, a)
f x = let %1 y = x ; %1 z = y in (y, y)
