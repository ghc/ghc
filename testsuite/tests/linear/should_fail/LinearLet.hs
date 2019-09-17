{-# LANGUAGE LinearTypes #-}
module LinearLet where

f :: a ->. (a,a)
f x = let y = x in (y,y)
