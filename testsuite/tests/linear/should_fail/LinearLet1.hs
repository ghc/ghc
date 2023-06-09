{-# LANGUAGE LinearTypes #-}
module LinearLet where

f :: a %1 -> (a,a)
f x = let y = x in (y,y)
