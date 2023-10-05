{-# LANGUAGE ViewPatterns, BangPatterns #-}
module InTreeAnnotations1 where

foo a@(_,_) !"a" ~x = undefined

data T = MkT { x,y :: Int }

f (MkT { x = !v, y = negate -> w }) = v + w
