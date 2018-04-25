{-# LANGUAGE ViewPatterns, BangPatterns #-}

module T6038 where

data T = MkT { x,y :: Int }

f (MkT { x = !v, y = negate -> w }) = v + w
