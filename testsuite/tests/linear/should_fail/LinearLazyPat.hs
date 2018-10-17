{-# LANGUAGE LinearTypes #-}
module LinearLazyPat where

f :: (a,b) ->. (b,a)
f ~(x,y) = (y,x)
