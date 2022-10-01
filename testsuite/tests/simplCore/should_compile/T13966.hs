{-# LANGUAGE ExistentialQuantification #-}

module T13966 (chain) where

import Prelude hiding (sum, filter, enumFromTo)

data Stream a = forall s. MkStream !(s -> Step s a) !s
data Step s a = Done | Yield a !s -- NB: No Skip

sum :: Stream Int -> Int
sum (MkStream next s0) = loop_sum 0 s0
  where
    loop_sum !a !s = case next s of
      Done       -> a
      Yield x s' -> loop_sum (a + x) s'

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (MkStream next0 s0) = MkStream next s0
  where
    next !s = case next0 s of
      Done                   -> Done
      Yield x s' | p x       -> Yield x s'
                 | otherwise -> next    s'  -- NB: recursive call

enumFromTo :: Int -> Int -> Stream Int
enumFromTo x y = MkStream next x
  where
    next n
        | n > y     = Done
        | otherwise = Yield n (n+1)

chain :: Int -> Int
chain = sum . filter even . enumFromTo 1
