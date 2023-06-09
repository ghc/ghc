{-# LANGUAGE LinearTypes #-}
module LinearLet2 where

f :: a -> (a, a)
f x = let %1 y = x in (y, y)
