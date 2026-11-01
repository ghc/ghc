{-# LANGUAGE BangPatterns #-}
module StackLayoutCallLoop (countWith) where

-- On one path the loop calls f, whose continuation returns into the loop;
-- the other path makes no call.  Stack layout reserves the frame of that
-- continuation when the loop is entered (Note [Reserved frames in loops] in
-- GHC.Cmm.LayoutStack), so the call stores its return address at Sp: before,
-- the call set its frame up below Sp and the continuation popped it,
-- moving Sp twice an iteration of that path.
f :: Int -> Int
f x = x * x + 1
{-# NOINLINE f #-}

countWith :: Int -> Int -> Int
countWith !n !k = go 0 0
  where
    go !i !acc
      | i >= n = acc
      | i `rem` k == 0 = case f i of !y -> go (i + 1) (acc + y)
      | otherwise = go (i + 1) (acc + i)
