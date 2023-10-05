{-# LANGUAGE LinearTypes #-}
module LinearLazyPat where

f :: (a,b) %1 -> (b,a)
f ~(x,y) = (y,x)
