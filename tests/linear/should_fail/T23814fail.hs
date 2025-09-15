{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiWayIf #-}

module T23814fail where

f' :: Bool -> Int %1 -> Int
f' b x =
  if
    | b -> x
    | otherwise -> 0

g' :: Bool -> Bool -> Int %1 -> Int
g' b c x =
   if
     | b -> x
     | c -> 0
     | otherwise -> 0
