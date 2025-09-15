{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiWayIf #-}

module T23814 where

f :: Bool -> Int %1 -> Int
f b x =
  if
    | b -> x
    | otherwise -> x

g :: Bool -> Bool -> Int %1 -> Int %1 -> (Int, Int)
g b c x y =
  if
    | b -> (x,y)
    | c -> (y,x)
    | otherwise -> (x,y)
